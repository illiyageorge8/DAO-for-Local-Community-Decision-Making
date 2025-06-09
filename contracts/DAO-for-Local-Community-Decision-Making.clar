(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROPOSAL-EXISTS (err u101))
(define-constant ERR-NO-PROPOSAL (err u102))
(define-constant ERR-PROPOSAL-EXPIRED (err u103))
(define-constant ERR-ALREADY-VOTED (err u104))
(define-constant ERR-PROPOSAL-ACTIVE (err u105))
(define-constant VOTING_PERIOD u1440)

(define-data-var proposal-count uint u0)

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
        amount: uint
    }
)

(define-map Votes
    {proposal-id: uint, voter: principal}
    bool
)

(define-public (register-resident)
    (ok (map-set Residents tx-sender {verified: true, voting-power: u1}))
)

(define-public (submit-proposal (title (string-ascii 100)) (description (string-ascii 500)) (amount uint))
    (let ((proposal-id (var-get proposal-count)))
        (asserts! (is-some (map-get? Residents tx-sender)) ERR-NOT-AUTHORIZED)
        (map-set Proposals proposal-id
            {
                title: title,
                description: description,
                proposer: tx-sender,
                start-block: burn-block-height,
                yes-votes: u0,
                no-votes: u0,
                executed: false,
                amount: amount
            }
        )
        (var-set proposal-count (+ proposal-id u1))
        (ok proposal-id)
    )
)

(define-public (vote (proposal-id uint) (vote-bool bool))
    (let (
        (proposal (unwrap! (map-get? Proposals proposal-id) ERR-NO-PROPOSAL))
        (voter-info (unwrap! (map-get? Residents tx-sender) ERR-NOT-AUTHORIZED))
    )
        (asserts! (< (- burn-block-height (get start-block proposal)) VOTING_PERIOD) ERR-PROPOSAL-EXPIRED)
        (asserts! (is-none (map-get? Votes {proposal-id: proposal-id, voter: tx-sender})) ERR-ALREADY-VOTED)
        
        (map-set Votes {proposal-id: proposal-id, voter: tx-sender} vote-bool)
        
        (if vote-bool
            (map-set Proposals proposal-id 
                (merge proposal {yes-votes: (+ (get yes-votes proposal) (get voting-power voter-info))})
            )
            (map-set Proposals proposal-id 
                (merge proposal {no-votes: (+ (get no-votes proposal) (get voting-power voter-info))})
            )
        )
        (ok true)
    )
)

(define-public (execute-proposal (proposal-id uint))
    (let ((proposal (unwrap! (map-get? Proposals proposal-id) ERR-NO-PROPOSAL)))
        (asserts! (>= (- burn-block-height (get start-block proposal)) VOTING_PERIOD) ERR-PROPOSAL-ACTIVE)
        (asserts! (not (get executed proposal)) ERR-PROPOSAL-EXISTS)
        (asserts! (> (get yes-votes proposal) (get no-votes proposal)) ERR-NOT-AUTHORIZED)
        
        (map-set Proposals proposal-id (merge proposal {executed: true}))
        (ok true)
    )
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
