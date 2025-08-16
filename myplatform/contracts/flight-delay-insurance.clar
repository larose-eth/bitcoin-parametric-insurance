;; Flight Delay Parametric Insurance Contract
;; This contract implements parametric insurance policies that pay out based on flight delay and cancellation data
;; Automatically triggers payouts when flights are delayed beyond specified thresholds or cancelled

;; Error codes
(define-constant err-unauthorized (err u200))
(define-constant err-invalid-policy (err u201))
(define-constant err-policy-exists (err u202))
(define-constant err-policy-not-found (err u203))
(define-constant err-insufficient-funds (err u204))
(define-constant err-invalid-oracle (err u205))
(define-constant err-invalid-parameters (err u206))
(define-constant err-policy-expired (err u207))
(define-constant err-already-claimed (err u208))
(define-constant err-threshold-not-met (err u209))
(define-constant err-no-flight-data (err u210))
(define-constant err-flight-not-departed (err u211))
(define-constant err-invalid-flight-number (err u212))

;; Data variables
(define-data-var contract-owner principal tx-sender)
(define-data-var oracle-address principal tx-sender)
(define-data-var pool-balance uint u0)
(define-data-var policy-counter uint u0)

;; Flight status types
(define-constant flight-status-on-time u1)
(define-constant flight-status-delayed u2)
(define-constant flight-status-cancelled u3)
(define-constant flight-status-diverted u4)

;; Policy status
(define-constant policy-status-active u1)
(define-constant policy-status-expired u2)
(define-constant policy-status-claimed u3)

;; Delay severity levels for tiered payouts
(define-constant delay-minor u1)      ;; 30-60 minutes
(define-constant delay-moderate u2)   ;; 60-120 minutes  
(define-constant delay-major u3)      ;; 120+ minutes
(define-constant delay-cancelled u4)  ;; Flight cancelled

;; Policy data structure
(define-map flight-policies
  uint
  {
    owner: principal,
    premium: uint,
    coverage: uint,
    flight-number: (string-ascii 10),
    departure-date: uint, ;; Unix timestamp
    departure-airport: (string-ascii 4), ;; IATA code
    arrival-airport: (string-ascii 4),   ;; IATA code
    min-delay-minutes: uint, ;; Minimum delay to trigger payout
    tiered-payout: bool, ;; Whether to use tiered payouts based on delay severity
    start-block: uint,
    end-block: uint, ;; Policy expires 24 hours after scheduled departure
    status: uint
  }
)

;; Flight data submitted by oracle
(define-map flight-data
  { flight-number: (string-ascii 10), departure-date: uint }
  { 
    status: uint, ;; flight-status-*
    scheduled-departure: uint, ;; Unix timestamp
    actual-departure: uint,    ;; Unix timestamp (0 if not departed)
    delay-minutes: uint,
    last-updated: uint ;; Block height when data was last updated
  }
)

;; Claims tracking
(define-map flight-claims
  uint ;; policy-id
  { 
    claimed: bool, 
    payout-amount: uint, 
    delay-minutes: uint,
    flight-status: uint,
    claim-block: uint 
  }
)

;; Tiered payout percentages (out of 100)
(define-map payout-tiers
  uint ;; delay severity level
  uint ;; payout percentage
)

;; Initialize payout tiers
(map-set payout-tiers delay-minor u25)      ;; 25% payout for 30-60 min delays
(map-set payout-tiers delay-moderate u50)   ;; 50% payout for 60-120 min delays  
(map-set payout-tiers delay-major u75)      ;; 75% payout for 120+ min delays
(map-set payout-tiers delay-cancelled u100) ;; 100% payout for cancellations

;; Private functions

;; Check if caller is contract owner
(define-private (is-owner)
  (is-eq tx-sender (var-get contract-owner))
)

;; Check if caller is oracle
(define-private (is-oracle)
  (is-eq tx-sender (var-get oracle-address))
)

;; Check if policy exists
(define-private (flight-policy-exists (policy-id uint))
  (is-some (map-get? flight-policies policy-id))
)

;; Check if policy is active
(define-private (is-flight-policy-active (policy-id uint))
  (let ((policy (unwrap! (map-get? flight-policies policy-id) false)))
    (is-eq (get status policy) policy-status-active)
  )
)

;; Check if policy is expired
(define-private (is-flight-policy-expired (policy-id uint))
  (let ((policy (unwrap! (map-get? flight-policies policy-id) false))
        (current-block stacks-block-height))
    (> current-block (get end-block policy))
  )
)

;; Determine delay severity level
(define-private (get-delay-severity (delay-minutes uint) (is-cancelled bool))
  (if is-cancelled
      delay-cancelled
      (if (>= delay-minutes u120)
          delay-major
          (if (>= delay-minutes u60)
              delay-moderate
              (if (>= delay-minutes u30)
                  delay-minor
                  u0))))
)

;; Calculate tiered payout amount
(define-private (calculate-tiered-payout (coverage uint) (delay-minutes uint) (is-cancelled bool))
  (let ((severity (get-delay-severity delay-minutes is-cancelled)))
    (if (> severity u0)
        (let ((payout-percentage (unwrap! (map-get? payout-tiers severity) u0)))
          (/ (* coverage payout-percentage) u100))
        u0))
)

;; Validate flight number format (simplified)
(define-private (is-valid-flight-number (flight-number (string-ascii 10)))
  (> (len flight-number) u2)
)

;; Public functions

;; Create a new flight delay insurance policy
(define-public (create-flight-policy 
                (premium uint) 
                (coverage uint) 
                (flight-number (string-ascii 10))
                (departure-date uint)
                (departure-airport (string-ascii 4))
                (arrival-airport (string-ascii 4))
                (min-delay-minutes uint)
                (tiered-payout bool))
  (let ((policy-id (+ (var-get policy-counter) u1))
        (current-block stacks-block-height)
        ;; Policy expires 24 hours (144 blocks) after scheduled departure
        (end-block (+ current-block u144)))
    
    ;; Validate parameters
    (asserts! (> premium u0) err-invalid-parameters)
    (asserts! (> coverage u0) err-invalid-parameters)
    (asserts! (is-valid-flight-number flight-number) err-invalid-flight-number)
    (asserts! (> departure-date u0) err-invalid-parameters)
    (asserts! (>= min-delay-minutes u30) err-invalid-parameters) ;; Minimum 30 minutes
    (asserts! (> (len departure-airport) u2) err-invalid-parameters)
    (asserts! (> (len arrival-airport) u2) err-invalid-parameters)
    
    ;; Check if user has enough funds to pay premium
    (asserts! (>= (stx-get-balance tx-sender) premium) err-insufficient-funds)
    
    ;; Transfer premium to contract
    (unwrap! (stx-transfer? premium tx-sender (as-contract tx-sender)) err-insufficient-funds)
    
    ;; Update pool balance
    (var-set pool-balance (+ (var-get pool-balance) premium))
    
    ;; Create policy
    (map-set flight-policies policy-id {
      owner: tx-sender,
      premium: premium,
      coverage: coverage,
      flight-number: flight-number,
      departure-date: departure-date,
      departure-airport: departure-airport,
      arrival-airport: arrival-airport,
      min-delay-minutes: min-delay-minutes,
      tiered-payout: tiered-payout,
      start-block: current-block,
      end-block: end-block,
      status: policy-status-active
    })
    
    ;; Increment policy counter
    (var-set policy-counter policy-id)
    
    ;; Return policy ID
    (ok policy-id))
)

;; Submit flight data (only callable by oracle)
(define-public (submit-flight-data 
                (flight-number (string-ascii 10))
                (departure-date uint)
                (status uint)
                (scheduled-departure uint)
                (actual-departure uint)
                (delay-minutes uint))
  (let ((current-block stacks-block-height))
    
    ;; Ensure only oracle can submit data
    (asserts! (is-oracle) err-unauthorized)
    
    ;; Validate flight status
    (asserts! (or (is-eq status flight-status-on-time)
                 (is-eq status flight-status-delayed)
                 (is-eq status flight-status-cancelled)
                 (is-eq status flight-status-diverted))
             err-invalid-parameters)
    
    ;; Store flight data
    (map-set flight-data 
      { flight-number: flight-number, departure-date: departure-date }
      { 
        status: status,
        scheduled-departure: scheduled-departure,
        actual-departure: actual-departure,
        delay-minutes: delay-minutes,
        last-updated: current-block
      })
    
    (ok true))
)

;; Check if flight policy can be claimed
(define-public (check-flight-claim (policy-id uint))
  (let ((policy (unwrap! (map-get? flight-policies policy-id) err-policy-not-found))
        (current-block stacks-block-height)
        (claim-data (default-to { claimed: false, payout-amount: u0, delay-minutes: u0, flight-status: u0, claim-block: u0 } 
                               (map-get? flight-claims policy-id))))
    
    ;; Check if policy is active
    (asserts! (is-eq (get status policy) policy-status-active) 
             (if (is-eq (get status policy) policy-status-claimed)
                 err-already-claimed
                 err-policy-expired))
    
    ;; Check if policy has not expired
    (asserts! (<= current-block (get end-block policy)) err-policy-expired)
    
    ;; Check if policy owner is calling
    (asserts! (is-eq tx-sender (get owner policy)) err-unauthorized)
    
    ;; Check if claim has not been made already
    (asserts! (not (get claimed claim-data)) err-already-claimed)
    
    ;; Get flight data
    (let ((flight-info (map-get? flight-data 
                       { flight-number: (get flight-number policy), 
                         departure-date: (get departure-date policy) })))
      
      ;; Check if flight data exists
      (if (is-some flight-info)
          (let ((flight (unwrap-panic flight-info))
                (flight-status (get status flight))
                (delay-minutes (get delay-minutes flight))
                (min-delay (get min-delay-minutes policy)))
            
            ;; Check if flight qualifies for payout
            (if (or (is-eq flight-status flight-status-cancelled)
                    (and (is-eq flight-status flight-status-delayed)
                         (>= delay-minutes min-delay)))
                ;; Flight qualifies for payout
                (ok true)
                ;; Flight does not qualify
                err-threshold-not-met))
          
          ;; No flight data available
          err-no-flight-data))
  )
)

;; Claim flight policy payout
(define-public (claim-flight-policy (policy-id uint))
  (let ((policy (unwrap! (map-get? flight-policies policy-id) err-policy-not-found))
        (current-block stacks-block-height))
    
    ;; Check if policy is active
    (asserts! (is-eq (get status policy) policy-status-active) 
             (if (is-eq (get status policy) policy-status-claimed)
                 err-already-claimed
                 err-policy-expired))
    
    ;; Check if policy has not expired
    (asserts! (<= current-block (get end-block policy)) err-policy-expired)
    
    ;; Check if policy owner is calling
    (asserts! (is-eq tx-sender (get owner policy)) err-unauthorized)
    
    ;; Get flight data
    (let ((flight-info (unwrap! (map-get? flight-data 
                                { flight-number: (get flight-number policy), 
                                  departure-date: (get departure-date policy) })
                               err-no-flight-data)))
      
      (let ((flight-status (get status flight-info))
            (delay-minutes (get delay-minutes flight-info))
            (min-delay (get min-delay-minutes policy))
            (coverage (get coverage policy))
            (use-tiered (get tiered-payout policy)))
        
        ;; Check if flight qualifies for payout
        (asserts! (or (is-eq flight-status flight-status-cancelled)
                      (and (is-eq flight-status flight-status-delayed)
                           (>= delay-minutes min-delay)))
                 err-threshold-not-met)
        
        ;; Calculate payout amount
        (let ((payout-amount (if use-tiered
                                (calculate-tiered-payout coverage delay-minutes 
                                                       (is-eq flight-status flight-status-cancelled))
                                coverage)))
          
          ;; Check if contract has enough funds
          (asserts! (>= (var-get pool-balance) payout-amount) err-insufficient-funds)
          
          ;; Update policy status
          (map-set flight-policies policy-id (merge policy { status: policy-status-claimed }))
          
          ;; Record claim
          (map-set flight-claims policy-id { 
            claimed: true, 
            payout-amount: payout-amount,
            delay-minutes: delay-minutes,
            flight-status: flight-status,
            claim-block: current-block 
          })
          
          ;; Update pool balance
          (var-set pool-balance (- (var-get pool-balance) payout-amount))
          
          ;; Transfer payout to policy owner
          (unwrap! (as-contract (stx-transfer? payout-amount tx-sender (get owner policy))) err-insufficient-funds)
          
          (ok payout-amount))))
  )
)

;; Calculate flight insurance premium based on risk factors
(define-public (calculate-flight-premium 
                (flight-number (string-ascii 10))
                (departure-airport (string-ascii 4))
                (arrival-airport (string-ascii 4))
                (coverage uint)
                (min-delay-minutes uint)
                (tiered-payout bool))
  (let (
    ;; Base premium rate (3%)
    (base-rate u30)
    ;; Airport risk factor (simplified - major hubs have higher delay rates)
    (departure-risk (if (or (is-eq departure-airport "JFK") 
                           (is-eq departure-airport "LAX")
                           (is-eq departure-airport "ORD")) u15 u10))
    (arrival-risk (if (or (is-eq arrival-airport "JFK") 
                         (is-eq arrival-airport "LAX")
                         (is-eq arrival-airport "ORD")) u10 u5))
    ;; Sensitivity factor (lower thresholds = higher premiums)
    (threshold-factor (if (< min-delay-minutes u60) u20 u10))
    ;; Tiered payout discount (tiered payouts reduce risk)
    (tiered-discount (if tiered-payout u5 u0))
  )
    ;; Calculate premium: (base-rate + departure-risk + arrival-risk + threshold-factor - tiered-discount) * coverage / 1000
    (ok (/ (* (- (+ (+ (+ base-rate departure-risk) arrival-risk) threshold-factor) tiered-discount) coverage) u1000)))
)

;; Admin functions

;; Set oracle address (only owner)
(define-public (set-flight-oracle (new-oracle principal))
  (let ((is-authorized (is-owner)))
    (asserts! is-authorized err-unauthorized)
    (var-set oracle-address new-oracle)
    (ok true))
)

;; Update payout tier percentages (only owner)
(define-public (update-payout-tier (tier uint) (percentage uint))
  (let ((is-authorized (is-owner)))
    (asserts! is-authorized err-unauthorized)
    (asserts! (<= percentage u100) err-invalid-parameters)
    (map-set payout-tiers tier percentage)
    (ok true))
)

;; Transfer ownership (only owner)
(define-public (transfer-flight-ownership (new-owner principal))
  (let ((is-authorized (is-owner)))
    (asserts! is-authorized err-unauthorized)
    (var-set contract-owner new-owner)
    (ok true))
)

;; Read-only functions

;; Get flight policy details
(define-read-only (get-flight-policy (policy-id uint))
  (map-get? flight-policies policy-id)
)

;; Get flight data
(define-read-only (get-flight-data (flight-number (string-ascii 10)) (departure-date uint))
  (map-get? flight-data { flight-number: flight-number, departure-date: departure-date })
)

;; Get flight claim status
(define-read-only (get-flight-claim-status (policy-id uint))
  (map-get? flight-claims policy-id)
)

;; Get payout tier percentage
(define-read-only (get-payout-tier (tier uint))
  (map-get? payout-tiers tier)
)

;; Get contract owner
(define-read-only (get-flight-contract-owner)
  (var-get contract-owner)
)

;; Get oracle address
(define-read-only (get-flight-oracle)
  (var-get oracle-address)
)

;; Get pool balance
(define-read-only (get-flight-pool-balance)
  (var-get pool-balance)
)

;; Get policy counter
(define-read-only (get-flight-policy-counter)
  (var-get policy-counter)
)