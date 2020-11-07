module Transaction exposing (..)

import Time


type alias Id =
    Int


type alias Payee =
    { payeeType : PayeeType
    , payeeName : String
    , payeeEmail : String
    , payeeAddress : Address
    }


type alias Address =
    { street : String
    , city : String
    , province : String
    , postalCode : String
    }


type PayeeType
    = Mortgagee
    | LandTransfer
    | PropertyTax


payeeTypeToString : PayeeType -> String
payeeTypeToString pt =
    case pt of
        Mortgagee ->
            "Mortgagee"

        LandTransfer ->
            "Land Transfer"

        PropertyTax ->
            "Property Tax"


type PayMethod
    = Cheque
    | WireTransfer


payMethodToString : PayMethod -> String
payMethodToString pm =
    case pm of
        Cheque ->
            "Cheque"

        WireTransfer ->
            "Wire Transfer"


type alias Disbursement =
    { payee : Payee
    , payoutAmount : Float
    , payMethod : PayMethod
    , payAccount : String
    , completed : Maybe Time.Posix -- Nothing == disbursement not completed
    }


type TransStatus
    = Draft
    | DepositReq
    | SignatureReq
    | Completed


transStatusToString : TransStatus -> String
transStatusToString tst =
    case tst of
        Draft ->
            "Draft"

        DepositReq ->
            "Deposit Required"

        SignatureReq ->
            "Signature Required"

        Completed ->
            "Completed"



-- sort criteria for the transactions table


type TransSort
    = TypeSort
    | ClientSort
    | PropertySort
    | ClosingSort


transSortToString : TransSort -> String
transSortToString s =
    case s of
        TypeSort ->
            "Type"

        ClientSort ->
            "Client"

        PropertySort ->
            "Property"

        ClosingSort ->
            "Closing"



-- calculates the sorting function based on a TransSort value


sortFunction : TransSort -> (Transaction -> String)
sortFunction s =
    case s of
        TypeSort ->
            transTypeToString << .transType

        ClientSort ->
            .client

        PropertySort ->
            .property

        ClosingSort ->
            .closing


type TransType
    = Sale
    | Purchase



--| Refinance


transTypeToString : TransType -> String
transTypeToString tt =
    case tt of
        Sale ->
            "Sale"

        Purchase ->
            "Purchase"



--Refinance ->
--    "Refinance"


type Either a b
    = Left a
    | Right b



-- *** Must add deposits, other party lawyer, purchaser


type alias NewTransaction =
    { userId : Int
    , transType : TransType
    , client : String
    , property : String
    , closing : String
    , status : TransStatus
    , otherLawyer : String
    , otherClient : String -- the other person involved in transaction
    }


type alias Transaction =
    { id : Int
    , user : User
    , transType : TransType
    , client : String
    , property : String
    , closing : String
    , status : TransStatus
    , disbursements : List Disbursement
    }


type TransFilter
    = NoFilter
    | FilterById Id
    | FilterByString String
    | FilterBoth Id String



-- predicate for filtering transactions as belonging to current logged in user


belongsToId : Id -> Transaction -> Bool
belongsToId id t =
    id == t.user.userId



-- helper to do search based on entered text in the search field


transactionToString : Transaction -> String
transactionToString t =
    String.join " " [ t.client, t.property, t.closing ] |> String.toLower


transactionContains : String -> Transaction -> Bool
transactionContains str t =
    String.contains (String.toLower str) (transactionToString t)


filteredTransactions : TransFilter -> List Transaction -> List Transaction
filteredTransactions tf lts =
    case tf of
        NoFilter ->
            lts

        FilterById id ->
            List.filter (belongsToId id) lts

        FilterByString str ->
            List.filter (transactionContains str) lts

        FilterBoth id str ->
            lts
                |> List.filter (belongsToId id)
                |> List.filter (transactionContains str)


selectTransaction : Int -> List Transaction -> Maybe Transaction
selectTransaction id lts =
    case lts of
        [] ->
            Nothing

        t :: rest ->
            if id == t.id then
                Just t

            else
                selectTransaction id rest


type alias User =
    { userId : Int
    , orgId : Int
    , name : String
    , email : String

    --, password : String
    }



--DATA


sam : User
sam =
    User 1 17 "Sam Klein" "sam@test.com"


keith : User
keith =
    User 2 17 "Keith Roy" "keith@test.com"


initialUsers : List User
initialUsers =
    [ sam -- currently logged user
    , keith
    ]


initialTransactions : List Transaction
initialTransactions =
    [ Transaction 1 sam Sale "Petra Morgan" "123 Water Street" "2020/09/20" Draft []
    , Transaction 2 keith Purchase "Kaila Hemsworth" "4518 Lockhart Dr." "2020/08/13" DepositReq []
    , Transaction 3 sam Purchase "Imran Harper" "2001 Falon Drive" "2022/06/27" Draft []
    , Transaction 4 keith Sale "Cheyene Booker" "2290 Island Hwy" "2023/11/05" SignatureReq []
    , Transaction 5 sam Purchase "Aaron Humphries" "2872 Rogers Road" "2021/07/09" Draft []
    , Transaction 6 keith Sale "Karol Johnson" "73 Somerset Dr." "2021/01/01" DepositReq []
    , Transaction 7 sam Purchase "Johnny Cash" "171 Simcoe Lake Road" "2020/12/19" Draft []
    ]


payee1 : Payee
payee1 =
    { payeeName = "TD Bank"
    , payeeType = Mortgagee
    , payeeEmail = "td_office@tdb.ca"
    , payeeAddress =
        { city = "Vancouver"
        , street = "123 Harbour Street"
        , postalCode = "V0V 0V0"
        , province = "British Columbia"
        }
    }


payee2 : Payee
payee2 =
    { payeeName = "RBC Royal Bank"
    , payeeType = Mortgagee
    , payeeEmail = "rbc_tor@rbc.ca"
    , payeeAddress =
        { city = "Toronto"
        , street = "731 Yonge Street"
        , postalCode = "M17 7G3"
        , province = "Ontario"
        }
    }


payee3 : Payee
payee3 =
    { payeeName = "Toronto Real Estate Registry"
    , payeeType = LandTransfer
    , payeeEmail = "tor.lreg@services.ca"
    , payeeAddress =
        { city = "Toronto"
        , street = "127 University Boulevard"
        , postalCode = "M3F 7A9"
        , province = "Ontario"
        }
    }
