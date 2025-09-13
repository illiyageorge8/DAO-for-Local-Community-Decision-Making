(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROPOSAL-EXISTS (err u101))
(define-constant ERR-NO-PROPOSAL (err u102))
(define-constant ERR-PROPOSAL-EXPIRED (err u103))
(define-constant ERR-ALREADY-VOTED (err u104))
(define-constant ERR-PROPOSAL-ACTIVE (err u105))
(define-constant ERR-CANNOT-DELEGATE-TO-SELF (err u106))
(define-constant ERR-DELEGATE-NOT-RESIDENT (err u107))
(define-constant ERR-QUORUM-NOT-MET (err u108))
(define-constant ERR-INVALID-CATEGORY (err u109))
(define-constant ERR-TIMELOCK-ACTIVE (err u110))
(define-constant ERR-TIMELOCK-NOT-EXPIRED (err u111))
(define-constant ERR-AMENDMENT-LIMIT-REACHED (err u112))
(define-constant ERR-NO-AMENDMENTS-ALLOWED (err u113))
(define-constant ERR-AMENDMENT-TOO-LATE (err u114))
(define-constant VOTING_PERIOD u1440)
(define-constant TIMELOCK_PERIOD u720)
(define-constant MAX-AMENDMENTS u3)
(define-constant AMENDMENT-WINDOW u1152)

(define-constant CATEGORY-BUDGET u1)
(define-constant CATEGORY-INFRASTRUCTURE u2)
(define-constant CATEGORY-COMMUNITY u3)
(define-constant CATEGORY-GOVERNANCE u4)

(define-data-var proposal-count uint u0)
(define-data-var total-residents uint u0)

(define-map Residents 
    principal 
    {verified: bool, voting-power: uint}
)

(define-map Proposals
    uint 
    {
        title: (string-ascii 100),
        description: (string-ascii 500),
        proposer: principal,
        start-block: uint,
        yes-votes: uint,
        no-votes: uint,
        executed: bool,
        amount: uint,
        category: uint,
        timelock-end: (optional uint),
        amendment-count: uint,
        last-amendment-block: (optional uint)
    }
)

(define-map Votes
    {proposal-id: uint, voter: principal}
    bool
)

(define-map Delegations
    principal
    principal
)

(define-map CategoryQuorums
    uint
    uint
)

(define-map Amendments
    {proposal-id: uint, amendment-index: uint}
    {
        title: (string-ascii 100),
        description: (string-ascii 500),
        amount: uint,
        amendment-block: uint
    }
)

(define-private (initialize-quorums)
    (begin
        (map-set CategoryQuorums CATEGORY-BUDGET u60)
        (map-set CategoryQuorums CATEGORY-INFRASTRUCTURE u50)
        (map-set CategoryQuorums CATEGORY-COMMUNITY u30)
        (map-set CategoryQuorums CATEGORY-GOVERNANCE u70)
    )
)

(define-public (register-resident)
    (begin
        (map-set Residents tx-sender {verified: true, voting-power: u1})
        (var-set total-residents (+ (var-get total-residents) u1))
        (ok true)
    )
)

(define-public (delegate-vote (delegate-to principal))
    (begin
        (asserts! (is-some (map-get? Residents tx-sender)) ERR-NOT-AUTHORIZED)
        (asserts! (is-some (map-get? Residents delegate-to)) ERR-DELEGATE-NOT-RESIDENT)
        (asserts! (not (is-eq tx-sender delegate-to)) ERR-CANNOT-DELEGATE-TO-SELF)
        (map-set Delegations tx-sender delegate-to)
        (ok true)
    )
)

(define-public (undelegate-vote)
    (begin
        (asserts! (is-some (map-get? Residents tx-sender)) ERR-NOT-AUTHORIZED)
        (map-delete Delegations tx-sender)
        (ok true)
    )
)

(define-private (get-total-voting-power (voter principal))
    (let (
        (base-power (default-to u0 (get voting-power (map-get? Residents voter))))
    )
        base-power
    )
)

(define-private (get-delegated-power-from (delegator principal))
    (default-to u0 (get voting-power (map-get? Residents delegator)))
)

(define-private (get-all-delegators (delegate principal))
    (list)
)

(define-public (submit-proposal (title (string-ascii 100)) (description (string-ascii 500)) (amount uint) (category uint))
    (let ((proposal-id (var-get proposal-count)))
        (asserts! (is-some (map-get? Residents tx-sender)) ERR-NOT-AUTHORIZED)
        (asserts! (is-valid-category category) ERR-INVALID-CATEGORY)
        (map-set Proposals proposal-id
            {
                title: title,
                description: description,
                proposer: tx-sender,
                start-block: burn-block-height,
                yes-votes: u0,
                no-votes: u0,
                executed: false,
                amount: amount,
                category: category,
                timelock-end: none,
                amendment-count: u0,
                last-amendment-block: none
            }
        )
        (var-set proposal-count (+ proposal-id u1))
        (ok proposal-id)
    )
)

(define-public (amend-proposal (proposal-id uint) (new-title (string-ascii 100)) (new-description (string-ascii 500)) (new-amount uint))
    (let (
        (proposal (unwrap! (map-get? Proposals proposal-id) ERR-NO-PROPOSAL))
        (amendment-count (get amendment-count proposal))
        (blocks-since-start (- burn-block-height (get start-block proposal)))
        (blocks-since-last-amendment (match (get last-amendment-block proposal)
            last-block (- burn-block-height last-block)
            u0
        ))
    )
        (asserts! (is-eq tx-sender (get proposer proposal)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get executed proposal)) ERR-PROPOSAL-EXISTS)
        (asserts! (is-none (get timelock-end proposal)) ERR-TIMELOCK-ACTIVE)
        (asserts! (< blocks-since-start AMENDMENT-WINDOW) ERR-AMENDMENT-TOO-LATE)
        (asserts! (< amendment-count MAX-AMENDMENTS) ERR-AMENDMENT-LIMIT-REACHED)
        (asserts! (< blocks-since-start VOTING_PERIOD) ERR-PROPOSAL-EXPIRED)
        
        (map-set Amendments {proposal-id: proposal-id, amendment-index: amendment-count}
            {
                title: new-title,
                description: new-description,
                amount: new-amount,
                amendment-block: burn-block-height
            }
        )
        
        (map-set Proposals proposal-id
            (merge proposal {
                title: new-title,
                description: new-description,
                amount: new-amount,
                amendment-count: (+ amendment-count u1),
                last-amendment-block: (some burn-block-height),
                yes-votes: u0,
                no-votes: u0
            })
        )
        
        (ok amendment-count)
    )
)

(define-public (vote (proposal-id uint) (vote-bool bool))
    (let (
        (proposal (unwrap! (map-get? Proposals proposal-id) ERR-NO-PROPOSAL))
        (voter-info (unwrap! (map-get? Residents tx-sender) ERR-NOT-AUTHORIZED))
        (effective-voter tx-sender)
        (total-power (get-total-voting-power effective-voter))
    )
        (asserts! (< (- burn-block-height (get start-block proposal)) VOTING_PERIOD) ERR-PROPOSAL-EXPIRED)
        (asserts! (is-none (map-get? Votes {proposal-id: proposal-id, voter: effective-voter})) ERR-ALREADY-VOTED)
        
        (map-set Votes {proposal-id: proposal-id, voter: effective-voter} vote-bool)
        
        (if vote-bool
            (map-set Proposals proposal-id 
                (merge proposal {yes-votes: (+ (get yes-votes proposal) total-power)})
            )
            (map-set Proposals proposal-id 
                (merge proposal {no-votes: (+ (get no-votes proposal) total-power)})
            )
        )
        (ok true)
    )
)

(define-public (queue-proposal (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? Proposals proposal-id) ERR-NO-PROPOSAL))
        (total-votes (+ (get yes-votes proposal) (get no-votes proposal)))
        (required-quorum (get-required-quorum (get category proposal)))
        (participation-rate (/ (* total-votes u100) (var-get total-residents)))
        (timelock-end-block (+ burn-block-height TIMELOCK_PERIOD))
    )
        (asserts! (>= (- burn-block-height (get start-block proposal)) VOTING_PERIOD) ERR-PROPOSAL-ACTIVE)
        (asserts! (not (get executed proposal)) ERR-PROPOSAL-EXISTS)
        (asserts! (is-none (get timelock-end proposal)) ERR-TIMELOCK-ACTIVE)
        (asserts! (> (get yes-votes proposal) (get no-votes proposal)) ERR-NOT-AUTHORIZED)
        (asserts! (>= participation-rate required-quorum) ERR-QUORUM-NOT-MET)
        
        (map-set Proposals proposal-id (merge proposal {timelock-end: (some timelock-end-block)}))
        (ok timelock-end-block)
    )
)

(define-public (execute-proposal (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? Proposals proposal-id) ERR-NO-PROPOSAL))
        (timelock-end (unwrap! (get timelock-end proposal) ERR-TIMELOCK-ACTIVE))
    )
        (asserts! (not (get executed proposal)) ERR-PROPOSAL-EXISTS)
        (asserts! (>= burn-block-height timelock-end) ERR-TIMELOCK-NOT-EXPIRED)
        
        (map-set Proposals proposal-id (merge proposal {executed: true}))
        (ok true)
    )
)

(define-private (is-valid-category (category uint))
    (or (is-eq category CATEGORY-BUDGET)
        (or (is-eq category CATEGORY-INFRASTRUCTURE)
            (or (is-eq category CATEGORY-COMMUNITY)
                (is-eq category CATEGORY-GOVERNANCE))))
)

(define-private (get-required-quorum (category uint))
    (default-to u50 (map-get? CategoryQuorums category))
)

(define-read-only (get-proposal (proposal-id uint))
    (map-get? Proposals proposal-id)
)

(define-read-only (get-resident (address principal))
    (map-get? Residents address)
)

(define-read-only (has-voted (proposal-id uint) (voter principal))
    (is-some (map-get? Votes {proposal-id: proposal-id, voter: voter}))
)

(define-read-only (get-delegation (delegator principal))
    (map-get? Delegations delegator)
)

(define-read-only (get-proposal-participation (proposal-id uint))
    (match (map-get? Proposals proposal-id)
        proposal (let (
            (total-votes (+ (get yes-votes proposal) (get no-votes proposal)))
        )
            (some (/ (* total-votes u100) (var-get total-residents)))
        )
        none
    )
)

(define-read-only (get-category-quorum (category uint))
    (map-get? CategoryQuorums category)
)

(define-read-only (get-amendment (proposal-id uint) (amendment-index uint))
    (map-get? Amendments {proposal-id: proposal-id, amendment-index: amendment-index})
)

(define-read-only (get-amendment-count (proposal-id uint))
    (match (map-get? Proposals proposal-id)
        proposal (some (get amendment-count proposal))
        none
    )
)

(define-read-only (can-amend-proposal (proposal-id uint))
    (match (map-get? Proposals proposal-id)
        proposal (let (
            (blocks-since-start (- burn-block-height (get start-block proposal)))
            (amendment-count (get amendment-count proposal))
        )
            (and 
                (not (get executed proposal))
                (is-none (get timelock-end proposal))
                (< blocks-since-start AMENDMENT-WINDOW)
                (< amendment-count MAX-AMENDMENTS)
                (< blocks-since-start VOTING_PERIOD)
            )
        )
        false
    )
)

(define-read-only (get-timelock-status (proposal-id uint))
    (match (map-get? Proposals proposal-id)
        proposal (let (
            (timelock-end (get timelock-end proposal))
        )
            (if (is-some timelock-end)
                (if (>= burn-block-height (unwrap-panic timelock-end))
                    (some "ready")
                    (some "locked")
                )
                (some "not-queued")
            )
        )
        none
    )
)

(initialize-quorums)
