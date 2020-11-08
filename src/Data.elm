module Data exposing (..)

--import Transaction exposing (..)


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
    , property = "171 Simcoe Lake Road"
    , closing = "2020/12/19"
    , status = Draft
    , otherLawyer = "Lawrence Olivier"
    , otherClient = "Cruela DeVille"
    , disbursements = []
    }


t6 : Transaction
t6 =
    { id = 6
    , user = keith
    , transType = Sale
    , client = "Karol Johnson"
    , property = "73 Somerset Dr."
    , closing = "2021/01/01"
    , status = DepositReq
    , otherLawyer = "Samuel Carter"
    , otherClient = "Donald Trump"
    , disbursements = []
    }


t5 : Transaction
t5 =
    { id = 5
    , user = sam
    , transType = Purchase
    , client = "Aaron Humphries"
    , property = "2872 Rogers Road"
    , closing = "2023/11/05"
    , status = Draft
    , otherLawyer = "Eduard Herman"
    , otherClient = "Mary Poppins"
    , disbursements = []
    }


t4 : Transaction
t4 =
    { id = 4
    , user = keith
    , transType = Sale
    , client = "Cheyene Booker"
    , property = "2290 Island Hwy"
    , closing = "2023/11/05"
    , status = SignatureReq
    , otherLawyer = "Roman Polanski"
    , otherClient = "Carlos Ramos"
    , disbursements = []
    }


t1 : Transaction
t1 =
    { id = 1
    , user = sam
    , transType = Sale
    , client = "Petra Morgan"
    , property = "123 Water Street"
    , closing = "2020/09/20"
    , status = Draft
    , otherLawyer = "Sam Livingstone"
    , otherClient = "Nancy Drew"
    , disbursements = []
    }


t2 : Transaction
t2 =
    { id = 2
    , user = keith
    , transType = Purchase
    , client = "Kaila Hemsworth"
    , property = "4518 Lockhart Dr."
    , closing = "2020/08/13"
    , status = DepositReq
    , otherLawyer = "John McIrvine"
    , otherClient = "Caroline Wozniaki"
    , disbursements = []
    }


t3 : Transaction
t3 =
    { id = 3
    , user = sam
    , transType = Purchase
    , client = "Imran Harper"
    , property = "2001 Falon Drive"
    , closing = "2022/06/07"
    , status = Draft
    , otherLawyer = "Mark Strong"
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
