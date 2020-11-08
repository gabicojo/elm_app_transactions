module Transaction exposing (..)

--import Data exposing (..)

import Time


type alias Id =
    Int


type alias Payee =
    { payeeType : PayeeType
    , payeeName : String
    , payeeEmail : String
    , payeeAddress : Address
    }


type alias Lawyer =
    { lawyerName : String
    , lawyerCompany : String
    , lawyerAddress : Address
    }


type alias Address =
    { street : String
    , city : String

    --, province : String
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
            \t -> t.property.street

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
    , property : Address
    , closing : String
    , status : TransStatus
    , otherLawyer : Lawyer
    , otherClient : String -- the other person involved in transaction
    , disbursements : List Disbursement
    }


type TrField
    = ClientF
    | OtherClientF



--setTransClient : String -> Transaction -> Transaction
--setTransClient str t =
--    { t | client = str }


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
    String.join " " [ t.client, t.property.street, t.property.city, t.closing ] |> String.toLower


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



--maxId : Id
--nextId : List Transaction -> Id
--nextId lts =
--    let lIds = List.map (\t -> t.id) lts
--    case lIds of
--        [] -> 1
--        _ -> 1 + max_ lIds


nextId : List Transaction -> Id
nextId lts =
    List.map (\t -> t.id) lts
        |> List.maximum
        |> Maybe.withDefault 0
        --|> Basics.floor
        |> (+) 1


type alias User =
    { userId : Int
    , orgId : Int
    , name : String
    , email : String

    --, password : String
    }



--*** DATA ***


address1 : Address
address1 =
    { street = "123 Don Mills Rd."
    , city = "Toronto"
    , postalCode = "M1V 2J7"
    }


address2 : Address
address2 =
    { street = "79 Faywood Boulevard"
    , city = "Vancouver"
    , postalCode = "V1Z 9P6"
    }


address3 : Address
address3 =
    { city = "Toronto"
    , street = "127 University Boulevard"
    , postalCode = "M3F 7A9"
    }


address4 : Address
address4 =
    { city = "Toronto"
    , street = "731 Yonge Street"
    , postalCode = "M17 7G3"
    }


address5 : Address
address5 =
    { city = "Vancouver"
    , street = "123 Harbour Street"
    , postalCode = "V0V 0V0"
    }


address6 : Address
address6 =
    { city = "Hamilton"
    , street = "17 Passage Way"
    , postalCode = "H3T 5B9"
    }


address7 : Address
address7 =
    { city = "Barrie"
    , street = "1 Commonwealth Rd."
    , postalCode = "B7R 1D5"
    }


emptyAddress : Address
emptyAddress =
    Address "" "" ""


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
    [ t1, t2, t3, t4, t5, t6, t7 ]


t7 : Transaction
t7 =
    { id = 7
    , user = sam
    , transType = Purchase
    , client = "Johnny Cash"
    , property = address7
    , closing = "2020/12/19"
    , status = Draft
    , otherLawyer = lawyer7
    , otherClient = "Cruela DeVille"
    , disbursements = []
    }


t6 : Transaction
t6 =
    { id = 6
    , user = keith
    , transType = Sale
    , client = "Karol Johnson"
    , property = address6
    , closing = "2021/01/01"
    , status = DepositReq
    , otherLawyer = lawyer6
    , otherClient = "Donald Trump"
    , disbursements = []
    }


t5 : Transaction
t5 =
    { id = 5
    , user = sam
    , transType = Purchase
    , client = "Aaron Humphries"
    , property = address5
    , closing = "2023/11/05"
    , status = Draft
    , otherLawyer = lawyer5
    , otherClient = "Mary Poppins"
    , disbursements = []
    }


t4 : Transaction
t4 =
    { id = 4
    , user = keith
    , transType = Sale
    , client = "Cheyene Booker"
    , property = address4
    , closing = "2023/11/05"
    , status = SignatureReq
    , otherLawyer = lawyer4
    , otherClient = "Carlos Ramos"
    , disbursements = []
    }


t1 : Transaction
t1 =
    { id = 1
    , user = sam
    , transType = Sale
    , client = "Petra Morgan"
    , property = address1
    , closing = "2020/09/20"
    , status = Draft
    , otherLawyer = lawyer1
    , otherClient = "Nancy Drew"
    , disbursements = []
    }


t2 : Transaction
t2 =
    { id = 2
    , user = keith
    , transType = Purchase
    , client = "Kaila Hemsworth"
    , property = address2
    , closing = "2020/08/13"
    , status = DepositReq
    , otherLawyer = lawyer2
    , otherClient = "Caroline Wozniaki"
    , disbursements = []
    }


t3 : Transaction
t3 =
    { id = 3
    , user = sam
    , transType = Purchase
    , client = "Imran Harper"
    , property = address3
    , closing = "2022/06/07"
    , status = Draft
    , otherLawyer = lawyer3
    , otherClient = "Lily Loevens"
    , disbursements = []
    }



-- **** PAYEE


payee1 : Payee
payee1 =
    { payeeName = "TD Bank"
    , payeeType = Mortgagee
    , payeeEmail = "td_office@tdb.ca"
    , payeeAddress =
        { city = "Vancouver"
        , street = "123 Harbour Street"
        , postalCode = "V0V 0V0"
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
        }
    }


payee4 : Payee
payee4 =
    { payeeType = PropertyTax
    , payeeName = "TD Bank"
    , payeeEmail = "td_bank@tdb.ca"
    , payeeAddress = address2
    }


payee5 : Payee
payee5 =
    { payeeType = Mortgagee
    , payeeName = "RBC Royal Bank"
    , payeeEmail = "office@rbc.org"
    , payeeAddress = address1
    }


lawyer1 : Lawyer
lawyer1 =
    { lawyerName = "Mark Strong"
    , lawyerCompany = "Barnes & Associates"
    , lawyerAddress = address7
    }


lawyer2 : Lawyer
lawyer2 =
    { lawyerName = "Karl Letonja"
    , lawyerCompany = "Properties LLC."
    , lawyerAddress = address1
    }


lawyer3 : Lawyer
lawyer3 =
    { lawyerName = "John McIrvine"
    , lawyerCompany = "Properties LLC."
    , lawyerAddress = address2
    }


lawyer4 : Lawyer
lawyer4 =
    { lawyerName = "John McIrvine"
    , lawyerCompany = "Properties LLC."
    , lawyerAddress = address5
    }


lawyer5 : Lawyer
lawyer5 =
    { lawyerName = "Eduard Herman"
    , lawyerCompany = "Properties LLC."
    , lawyerAddress = address7
    }


lawyer6 : Lawyer
lawyer6 =
    { lawyerName = "Samuel Carter"
    , lawyerCompany = "Properties LLC."
    , lawyerAddress = address4
    }


lawyer7 : Lawyer
lawyer7 =
    { lawyerName = "Roman Polanski"
    , lawyerCompany = "Properties LLC."
    , lawyerAddress = address3
    }


emptyLawyer : Lawyer
emptyLawyer =
    { lawyerName = ""
    , lawyerCompany = ""
    , lawyerAddress = emptyAddress
    }
