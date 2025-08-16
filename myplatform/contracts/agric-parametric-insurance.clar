;; Agricultural/Crop Yield Parametric Insurance Contract
;; Provides insurance coverage for crop yield shortfalls based on oracle-verified harvest data

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_POLICY_NOT_FOUND (err u404))
(define-constant ERR_INVALID_PARAMETERS (err u400))
(define-constant ERR_POLICY_EXPIRED (err u410))
(define-constant ERR_POLICY_ALREADY_EXISTS (err u409))
(define-constant ERR_INSUFFICIENT_PREMIUM (err u402))
(define-constant ERR_CLAIM_ALREADY_PROCESSED (err u411))
(define-constant ERR_INVALID_CROP_TYPE (err u412))
(define-constant ERR_INVALID_YIELD_DATA (err u413))
(define-constant ERR_POLICY_NOT_ACTIVE (err u414))
(define-constant ERR_HARVEST_PERIOD_NOT_ENDED (err u415))

;; Crop types
(define-constant CROP_CORN u1)
(define-constant CROP_WHEAT u2)
(define-constant CROP_SOYBEANS u3)
(define-constant CROP_RICE u4)
(define-constant CROP_COTTON u5)

;; Payout tiers based on yield shortfall percentage
(define-constant TIER_25_PERCENT u25)
(define-constant TIER_50_PERCENT u50)
(define-constant TIER_75_PERCENT u75)
(define-constant TIER_100_PERCENT u100)

;; Data Variables
(define-data-var contract-active bool true)
(define-data-var total-policies-issued uint u0)
(define-data-var total-premiums-collected uint u0)
(define-data-var total-claims-paid uint u0)
(define-data-var oracle-address (optional principal) none)

;; Data Maps
(define-map policies
  { policy-id: uint }
  {
    farmer: principal,
    crop-type: uint,
    coverage-amount: uint,
    premium-paid: uint,
    expected-yield: uint, ;; bushels per acre
    acres-covered: uint,
    region: (string-ascii 50),
    planting-date: uint,
    harvest-date: uint,
    policy-start: uint,
    policy-end: uint,
    is-active: bool,
    claim-processed: bool
  }
)

(define-map crop-risk-factors
  { crop-type: uint, region: (string-ascii 50) }
  {
    base-risk-multiplier: uint, ;; basis points (10000 = 100%)
    historical-yield-avg: uint,
    volatility-factor: uint
  }
)

(define-map yield-reports
  { policy-id: uint }
  {
    actual-yield: uint, ;; bushels per acre
    total-production: uint,
    report-date: uint,
    oracle-verified: bool,
    shortfall-percentage: uint
  }
)

(define-map farmer-policies
  { farmer: principal }
  { policy-ids: (list 50 uint) }
)

;; Read-only functions
(define-read-only (get-policy (policy-id uint))
  (map-get? policies { policy-id: policy-id })
)

(define-read-only (get-yield-report (policy-id uint))
  (map-get? yield-reports { policy-id: policy-id })
)

(define-read-only (get-crop-risk-factors (crop-type uint) (region (string-ascii 50)))
  (map-get? crop-risk-factors { crop-type: crop-type, region: region })
)

(define-read-only (get-farmer-policies (farmer principal))
  (default-to { policy-ids: (list) } (map-get? farmer-policies { farmer: farmer }))
)

(define-read-only (get-contract-stats)
  {
    total-policies: (var-get total-policies-issued),
    total-premiums: (var-get total-premiums-collected),
    total-claims: (var-get total-claims-paid),
    contract-active: (var-get contract-active)
  }
)

(define-read-only (is-valid-crop-type (crop-type uint))
  (or (is-eq crop-type CROP_CORN)
      (or (is-eq crop-type CROP_WHEAT)
          (or (is-eq crop-type CROP_SOYBEANS)
              (or (is-eq crop-type CROP_RICE)
                  (is-eq crop-type CROP_COTTON)))))
)

(define-read-only (calculate-premium (crop-type uint) (region (string-ascii 50)) (coverage-amount uint) (acres uint))
  (let (
    (risk-data (get-crop-risk-factors crop-type region))
    (base-rate u200) ;; 2% base rate in basis points
  )
    (match risk-data
      risk-factors
      (let (
        (risk-multiplier (get base-risk-multiplier risk-factors))
        (volatility (get volatility-factor risk-factors))
        (adjusted-rate (+ base-rate (/ (* risk-multiplier volatility) u10000)))
        (premium-per-acre (/ (* coverage-amount adjusted-rate) u10000))
      )
        (ok (* premium-per-acre acres)))
      (ok (/ (* coverage-amount base-rate acres) u10000))
    )
  )
)

;; Private functions
(define-private (is-harvest-period-ended (policy-id uint))
  (match (get-policy policy-id)
    policy-data
    (>= stacks-block-height (get harvest-date policy-data))
    false
  )
)

(define-private (calculate-payout (coverage-amount uint) (shortfall-percentage uint))
  (if (>= shortfall-percentage u75)
    coverage-amount
    (if (>= shortfall-percentage u50)
      (/ (* coverage-amount u75) u100)
      (if (>= shortfall-percentage u25)
        (/ (* coverage-amount u50) u100)
        (if (>= shortfall-percentage u10)
          (/ (* coverage-amount u25) u100)
          u0))))
)

(define-private (add-policy-to-farmer (farmer principal) (policy-id uint))
  (let (
    (current-policies (get policy-ids (get-farmer-policies farmer)))
    (updated-policies (unwrap! (as-max-len? (append current-policies policy-id) u50) (err u500)))
  )
    (map-set farmer-policies { farmer: farmer } { policy-ids: updated-policies })
    (ok true)
  )
)

;; Public functions
(define-public (create-policy 
  (crop-type uint)
  (coverage-amount uint)
  (expected-yield uint)
  (acres-covered uint)
  (region (string-ascii 50))
  (planting-date uint)
  (harvest-date uint)
  (policy-duration uint))
  (let (
    (policy-id (+ (var-get total-policies-issued) u1))
    (premium-calculation (unwrap! (calculate-premium crop-type region coverage-amount acres-covered) ERR_INVALID_PARAMETERS))
    (policy-start stacks-block-height)
    (policy-end (+ stacks-block-height policy-duration))
  )
    (asserts! (var-get contract-active) ERR_UNAUTHORIZED)
    (asserts! (is-valid-crop-type crop-type) ERR_INVALID_CROP_TYPE)
    (asserts! (> coverage-amount u0) ERR_INVALID_PARAMETERS)
    (asserts! (> expected-yield u0) ERR_INVALID_PARAMETERS)
    (asserts! (> acres-covered u0) ERR_INVALID_PARAMETERS)
    (asserts! (> harvest-date planting-date) ERR_INVALID_PARAMETERS)
    (asserts! (>= (stx-get-balance tx-sender) premium-calculation) ERR_INSUFFICIENT_PREMIUM)
    
    ;; Transfer premium
    (try! (stx-transfer? premium-calculation tx-sender (as-contract tx-sender)))
    
    ;; Create policy
    (map-set policies
      { policy-id: policy-id }
      {
        farmer: tx-sender,
        crop-type: crop-type,
        coverage-amount: coverage-amount,
        premium-paid: premium-calculation,
        expected-yield: expected-yield,
        acres-covered: acres-covered,
        region: region,
        planting-date: planting-date,
        harvest-date: harvest-date,
        policy-start: policy-start,
        policy-end: policy-end,
        is-active: true,
        claim-processed: false
      }
    )
    
    ;; Add to farmer's policy list
    (try! (add-policy-to-farmer tx-sender policy-id))
    
    ;; Update contract stats
    (var-set total-policies-issued policy-id)
    (var-set total-premiums-collected (+ (var-get total-premiums-collected) premium-calculation))
    
    (ok policy-id)
  )
)

(define-public (submit-yield-data 
  (policy-id uint)
  (actual-yield uint)
  (total-production uint))
  (let (
    (policy-data (unwrap! (get-policy policy-id) ERR_POLICY_NOT_FOUND))
    (expected-yield (get expected-yield policy-data))
    (shortfall-percentage (if (< actual-yield expected-yield)
                           (/ (* (- expected-yield actual-yield) u100) expected-yield)
                           u0))
  )
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) 
                  (is-some (var-get oracle-address))) ERR_UNAUTHORIZED)
    (asserts! (get is-active policy-data) ERR_POLICY_NOT_ACTIVE)
    (asserts! (is-harvest-period-ended policy-id) ERR_HARVEST_PERIOD_NOT_ENDED)
    (asserts! (> actual-yield u0) ERR_INVALID_YIELD_DATA)
    
    ;; Record yield report
    (map-set yield-reports
      { policy-id: policy-id }
      {
        actual-yield: actual-yield,
        total-production: total-production,
        report-date: stacks-block-height,
        oracle-verified: true,
        shortfall-percentage: shortfall-percentage
      }
    )
    
    (ok shortfall-percentage)
  )
)

(define-public (process-claim (policy-id uint))
  (let (
    (policy-data (unwrap! (get-policy policy-id) ERR_POLICY_NOT_FOUND))
    (yield-data (unwrap! (get-yield-report policy-id) ERR_INVALID_YIELD_DATA))
    (shortfall-percentage (get shortfall-percentage yield-data))
    (coverage-amount (get coverage-amount policy-data))
    (payout-amount (calculate-payout coverage-amount shortfall-percentage))
    (farmer (get farmer policy-data))
  )
    (asserts! (get is-active policy-data) ERR_POLICY_NOT_ACTIVE)
    (asserts! (not (get claim-processed policy-data)) ERR_CLAIM_ALREADY_PROCESSED)
    (asserts! (get oracle-verified yield-data) ERR_UNAUTHORIZED)
    (asserts! (> payout-amount u0) ERR_INVALID_PARAMETERS)
    
    ;; Process payout if there's a shortfall
    (if (> payout-amount u0)
      (begin
        (try! (as-contract (stx-transfer? payout-amount tx-sender farmer)))
        (var-set total-claims-paid (+ (var-get total-claims-paid) payout-amount))
      )
      true
    )
    
    ;; Mark claim as processed
    (map-set policies
      { policy-id: policy-id }
      (merge policy-data { claim-processed: true, is-active: false })
    )
    
    (ok payout-amount)
  )
)

;; Admin functions
(define-public (set-crop-risk-factors 
  (crop-type uint)
  (region (string-ascii 50))
  (base-risk-multiplier uint)
  (historical-yield-avg uint)
  (volatility-factor uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (is-valid-crop-type crop-type) ERR_INVALID_CROP_TYPE)
    
    (map-set crop-risk-factors
      { crop-type: crop-type, region: region }
      {
        base-risk-multiplier: base-risk-multiplier,
        historical-yield-avg: historical-yield-avg,
        volatility-factor: volatility-factor
      }
    )
    (ok true)
  )
)

(define-public (set-oracle-address (new-oracle principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set oracle-address (some new-oracle))
    (ok true)
  )
)

(define-public (toggle-contract-status)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set contract-active (not (var-get contract-active)))
    (ok (var-get contract-active))
  )
)

(define-public (emergency-withdraw (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (as-contract (stx-transfer? amount tx-sender CONTRACT_OWNER))
  )
)
