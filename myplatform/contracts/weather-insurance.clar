;; Weather-Based Parametric Insurance Contract
;; This contract implements parametric insurance policies that pay out based on weather data

;; Error codes
(define-constant err-unauthorized (err u100))
(define-constant err-invalid-policy (err u101))
(define-constant err-policy-exists (err u102))
(define-constant err-policy-not-found (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-invalid-oracle (err u105))
(define-constant err-invalid-parameters (err u106))
(define-constant err-policy-expired (err u107))
(define-constant err-already-claimed (err u108))
(define-constant err-threshold-not-met (err u109))
(define-constant err-no-weather-data (err u110))

;; Data variables
(define-data-var contract-owner principal tx-sender)
(define-data-var oracle-address principal tx-sender)
(define-data-var pool-balance uint u0)
(define-data-var policy-counter uint u0)

;; Weather event types
(define-constant weather-type-rainfall u1)
(define-constant weather-type-temperature u2)
(define-constant weather-type-wind-speed u3)

;; Policy status
(define-constant policy-status-active u1)
(define-constant policy-status-expired u2)
(define-constant policy-status-claimed u3)

;; Policy data structure
(define-map policies
  uint
  {
    owner: principal,
    premium: uint,
    coverage: uint,
    location: (string-ascii 14),
    weather-type: uint,
    threshold: uint,
    comparison: (string-ascii 2), ;; "gt" for greater than, "lt" for less than
    start-block: uint,
    end-block: uint,
    status: uint
  }
)

;; Weather data submitted by oracle
(define-map weather-data
  { block: uint, location: (string-ascii 14), weather-type: uint }
  { value: uint }
)

;; Claims tracking
(define-map claims
  uint ;; policy-id
  { claimed: bool, payout-amount: uint, block: uint }
)

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
(define-private (policy-exists (policy-id uint))
  (is-some (map-get? policies policy-id))
)

;; Check if policy is active
(define-private (is-policy-active (policy-id uint))
  (let ((policy (unwrap! (map-get? policies policy-id) false)))
    (is-eq (get status policy) policy-status-active)
  )
)

;; Check if policy is expired
(define-private (is-policy-expired (policy-id uint))
  (let ((policy (unwrap! (map-get? policies policy-id) false))
        (current-block stacks-block-height))
    (> current-block (get end-block policy))
  )
)

;; Check if threshold is met
(define-private (is-threshold-met (policy-id uint) (weather-value uint))
  (let ((policy (unwrap! (map-get? policies policy-id) false)))
    (let ((threshold (get threshold policy))
          (comparison (get comparison policy)))
      (if (is-eq comparison "gt")
          (> weather-value threshold)
          (< weather-value threshold))
    )
  )
)

;; Public functions

;; Create a new weather insurance policy
(define-public (create-policy 
                (premium uint) 
                (coverage uint) 
                (location (string-ascii 14)) 
                (weather-type uint) 
                (threshold uint) 
                (comparison (string-ascii 2)) 
                (duration uint))
  (let ((policy-id (+ (var-get policy-counter) u1))
        (current-block stacks-block-height)
        (end-block (+ stacks-block-height duration)))
    
    ;; Validate parameters
    (asserts! (> premium u0) err-invalid-parameters)
    (asserts! (> coverage u0) err-invalid-parameters)
    (asserts! (> duration u0) err-invalid-parameters)
    (asserts! (or (is-eq comparison "gt") (is-eq comparison "lt")) err-invalid-parameters)
    (asserts! (or (is-eq weather-type weather-type-rainfall) 
                 (is-eq weather-type weather-type-temperature) 
                 (is-eq weather-type weather-type-wind-speed)) 
             err-invalid-parameters)
    
    ;; Check if user has enough funds to pay premium
    (asserts! (>= (stx-get-balance tx-sender) premium) err-insufficient-funds)
    
    ;; Transfer premium to contract
    (unwrap! (stx-transfer? premium tx-sender (as-contract tx-sender)) err-insufficient-funds)
    
    ;; Update pool balance
    (var-set pool-balance (+ (var-get pool-balance) premium))
    
    ;; Create policy
    (map-set policies policy-id {
      owner: tx-sender,
      premium: premium,
      coverage: coverage,
      location: location,
      weather-type: weather-type,
      threshold: threshold,
      comparison: comparison,
      start-block: current-block,
      end-block: end-block,
      status: policy-status-active
    })
    
    ;; Increment policy counter
    (var-set policy-counter policy-id)
    
    ;; Return policy ID
    (ok policy-id))
)

;; Submit weather data (only callable by oracle)
(define-public (submit-weather-data 
                (location (string-ascii 14)) 
                (weather-type uint) 
                (value uint))
  (let ((current-block stacks-block-height))
    
    ;; Ensure only oracle can submit data
    (asserts! (is-oracle) err-unauthorized)
    
    ;; Store weather data
    (map-set weather-data 
      { block: current-block, location: location, weather-type: weather-type }
      { value: value })
    
    (ok true))
)

;; Check if policy can be claimed
(define-public (check-claim (policy-id uint))
  (let ((policy (unwrap! (map-get? policies policy-id) err-policy-not-found))
        (current-block stacks-block-height)
        (claim-data (default-to { claimed: false, payout-amount: u0, block: u0 } 
                               (map-get? claims policy-id))))
    
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
    
    ;; Get latest weather data for the location and type
    (let ((weather (map-get? weather-data 
                   { block: current-block, 
                     location: (get location policy), 
                     weather-type: (get weather-type policy) })))
      
      ;; If weather data exists for current block
      (if (is-some weather)
          (let ((weather-value (get value (unwrap-panic weather)))
                (threshold (get threshold policy))
                (comparison (get comparison policy)))
            
            ;; Check if threshold is met based on comparison type
            (if (or (and (is-eq comparison "gt") (> weather-value threshold))
                    (and (is-eq comparison "lt") (< weather-value threshold)))
                ;; Threshold met, policy can be claimed
                (ok true)
                ;; Threshold not met
                err-threshold-not-met))
          
          ;; No weather data for current block
          err-no-weather-data))
  )
)

;; Claim policy payout
(define-public (claim-policy (policy-id uint))
  (let ((policy (unwrap! (map-get? policies policy-id) err-policy-not-found))
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
    
    ;; Get latest weather data for the location and type
    (let ((weather (unwrap! (map-get? weather-data 
                            { block: current-block, 
                              location: (get location policy), 
                              weather-type: (get weather-type policy) })
                           err-no-weather-data)))
      
      (let ((weather-value (get value weather))
            (threshold (get threshold policy))
            (comparison (get comparison policy))
            (coverage (get coverage policy)))
        
        ;; Check if threshold is met based on comparison type
        (asserts! (or (and (is-eq comparison "gt") (> weather-value threshold))
                      (and (is-eq comparison "lt") (< weather-value threshold)))
                 err-threshold-not-met)
        
        ;; Check if contract has enough funds
        (asserts! (>= (var-get pool-balance) coverage) err-insufficient-funds)
        
        ;; Update policy status
        (map-set policies policy-id (merge policy { status: policy-status-claimed }))
        
        ;; Record claim
        (map-set claims policy-id { 
          claimed: true, 
          payout-amount: coverage, 
          block: current-block 
        })
        
        ;; Update pool balance
        (var-set pool-balance (- (var-get pool-balance) coverage))
        
        ;; Transfer payout to policy owner
        (unwrap! (as-contract (stx-transfer? coverage tx-sender (get owner policy))) err-insufficient-funds)
        
        (ok true)))
  )
)

;; Calculate premium based on risk parameters (simplified version)
(define-public (calculate-premium 
                (location (string-ascii 14)) 
                (weather-type uint) 
                (threshold uint) 
                (comparison (string-ascii 2)) 
                (coverage uint) 
                (duration uint))
  (let (
    ;; Base premium rate (5%)
    (base-rate u50)
    ;; Location factor (simplified)
    (location-factor (if (is-eq location "high-risk-area") u20 u10))
    ;; Weather type factor
    (type-factor (if (is-eq weather-type weather-type-wind-speed) u15 u10))
    ;; Duration factor (1% per 1000 blocks)
    (duration-factor (/ (* duration u10) u1000))
  )
    ;; Calculate premium: (base-rate + location-factor + type-factor + duration-factor) * coverage / 1000
    (ok (/ (* (+ (+ (+ base-rate location-factor) type-factor) duration-factor) coverage) u1000)))
)

;; Admin functions

;; Set oracle address (only owner)
(define-public (set-oracle (new-oracle principal))
  (let ((is-authorized (is-owner)))
    (asserts! is-authorized err-unauthorized)
    (var-set oracle-address new-oracle)
    (ok true))
)

;; Transfer ownership (only owner)
(define-public (transfer-ownership (new-owner principal))
  (let ((is-authorized (is-owner)))
    (asserts! is-authorized err-unauthorized)
    (var-set contract-owner new-owner)
    (ok true))
)

;; Read-only functions

;; Get policy details
(define-read-only (get-policy (policy-id uint))
  (map-get? policies policy-id)
)

;; Get weather data
(define-read-only (get-weather-data (block uint) (location (string-ascii 14)) (weather-type uint))
  (map-get? weather-data { block: block, location: location, weather-type: weather-type })
)

;; Get claim status
(define-read-only (get-claim-status (policy-id uint))
  (map-get? claims policy-id)
)

;; Get contract owner
(define-read-only (get-contract-owner)
  (var-get contract-owner)
)

;; Get oracle address
(define-read-only (get-oracle)
  (var-get oracle-address)
)

;; Get pool balance
(define-read-only (get-pool-balance)
  (var-get pool-balance)
)