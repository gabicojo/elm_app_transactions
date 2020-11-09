module Main exposing (main)

import Browser
import Html exposing (Html, a, b, br, button, col, colgroup, div, em, h2, h3, h5, hr, i, img, input, label, li, p, span, table, td, text, th, tr, ul)
import Html.Attributes exposing (alt, checked, class, classList, height, href, id, name, selected, src, style, type_, value, width)
import Html.Events exposing (onClick, onInput)
import MultipleListFilters exposing (..)
import Task
import Time
import Transaction exposing (..)


boolToString : Bool -> String
boolToString b =
    case b of
        True ->
            "True"

        False ->
            "False"



--INITIALIZING AND TYPES;


init : () -> ( Model, Cmd Msg )
init jsTime =
    ( { zone = Time.utc
      , time = Time.millisToPosix 0
      , users = initialUsers
      , loggedUserId = 1
      , viewJMOTs = False
      , searchField = ""
      , transactions = initialTransactions

      --, selTransaction = Just (Transaction 2 2 PurchaseSale "Kaila Hemsworth" "4518 Lockhart Dr." "2020/08/13" DepositReq False)
      , transSort = PropertySort
      , sortAscending = True
      , viewMode = ViewTransactionsList -- app starts with transactionsList view
      }
    , Task.perform AdjustTimeZone Time.here
    )


type Mode
    = Old
    | New


type Msg
    = AdjustTimeZone Time.Zone
    | SearchFieldEdited String
    | ClearSearchField
      --| ClearSelectedTransaction
      --| SetSelectedTrans Id
    | ToggledJmot
    | SetTransSort TransSort
    | SetViewMode ViewMode
    | ChangeStage Stage
    | EditTransaction TrField String -- edits the transaction held in ViewMode
    | DiscardEdits
    | SaveEdits
    | SaveEditsAndContinue


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix -- is this needed?
    , users : List User -- all users are part of the same organization
    , loggedUserId : Id
    , viewJMOTs : Bool
    , searchField : String
    , transactions : List Transaction -- organization transactions

    --, selTransaction : Maybe Transaction -- remove this; viewMode holds the active transaction
    , transSort : TransSort
    , sortAscending : Bool

    -- replaces routes in Element apps
    -- type that helps the main view function display different "components"
    -- based on viewMode
    -- reflects the app status:
    -- *** view trans list
    -- *** create and start editing a new transaction
    -- *** open and edit an existing transaction
    , viewMode : ViewMode
    }


type ViewMode
    = ViewTransactionsList
    | ViewT Stage Transaction
    | ViewNewT Stage Transaction Transaction -- second added to revert to previous state using Cancel


changeViewStage : Stage -> ViewMode -> ViewMode
changeViewStage s vm =
    case vm of
        ViewTransactionsList ->
            ViewTransactionsList

        ViewT _ t ->
            ViewT s t

        ViewNewT _ t te ->
            ViewNewT s t te


type Stage
    = Review
    | Property
    | Lawyers
    | Parties
    | Disbursement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- *** TRANSACTION EDITING ***
        -- discards block changes
        DiscardEdits ->
            case model.viewMode of
                ViewTransactionsList ->
                    ( model, Cmd.none )

                -- get the record from the list again
                ViewT _ t ->
                    let
                        foundT =
                            findTransById t.id model.transactions
                    in
                    case foundT of
                        Nothing ->
                            ( { model | viewMode = ViewTransactionsList }, Cmd.none )

                        Just tr ->
                            ( { model | viewMode = ViewT Review tr }, Cmd.none )

                ViewNewT _ t te ->
                    ( { model | viewMode = ViewNewT Review t t }, Cmd.none )

        -- existing transaction and new transaction get saved to transactions list
        SaveEdits ->
            case model.viewMode of
                ViewTransactionsList ->
                    ( model, Cmd.none )

                ViewT _ t ->
                    ( { model
                        | transactions = saveTransaction t model.transactions
                        , viewMode = ViewT Review t
                      }
                    , Cmd.none
                    )

                ViewNewT _ t te ->
                    ( { model
                        | transactions = saveTransaction t model.transactions
                        , viewMode = ViewT Review t
                      }
                    , Cmd.none
                    )

        SaveEditsAndContinue ->
            case model.viewMode of
                ViewNewT Property _ te ->
                    ( { model | viewMode = ViewNewT Lawyers te te }, Cmd.none )

                ViewNewT Lawyers _ te ->
                    ( { model | viewMode = ViewNewT Parties te te }, Cmd.none )

                ViewNewT Parties _ te ->
                    ( { model | viewMode = ViewNewT Review te te }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        EditTransaction field val ->
            ( { model | viewMode = updateTransaction field val model.viewMode }, Cmd.none )

        ChangeStage s ->
            ( { model | viewMode = changeViewStage s model.viewMode }, Cmd.none )

        AdjustTimeZone z ->
            ( { model | zone = z }, Cmd.none )

        SetViewMode vm ->
            ( { model | viewMode = vm }, Cmd.none )

        SetTransSort s ->
            -- if the sort is the same, toggle sortAscending
            -- if the sort is different, then switch sort and set sortAscending True
            if model.transSort == s then
                ( { model | sortAscending = not model.sortAscending }, Cmd.none )

            else
                ( { model | transSort = s, sortAscending = True }, Cmd.none )

        ToggledJmot ->
            ( { model | viewJMOTs = not model.viewJMOTs }, Cmd.none )

        SearchFieldEdited str ->
            ( { model | searchField = str }, Cmd.none )

        ClearSearchField ->
            ( { model | searchField = "" }, Cmd.none )


updateTransaction : TrField -> String -> ViewMode -> ViewMode
updateTransaction field val vm =
    case vm of
        ViewTransactionsList ->
            vm

        ViewT s t ->
            updateTransactionField field val t |> ViewT s

        ViewNewT s t te ->
            updateTransactionField field val te |> ViewNewT s t



-- VIEW
-- main role of view is to orchestrate what "components" are visible at any time
-- it does that by pattern matching viewMode field in model; like routing


view : Model -> Html Msg
view model =
    case model.viewMode of
        ViewTransactionsList ->
            div [ class "main" ]
                [ viewTransactionsSearch model
                , viewTransactionsTable model
                ]

        ViewT Review t ->
            div [ class "main" ]
                [ viewTop t
                , div [ class "grid-1-3" ]
                    [ viewBreadCrumb model.viewMode
                    , viewReview model.viewMode
                    ]
                ]

        ViewT Disbursement t ->
            div [ class "main" ]
                [ viewTop t
                , div [ class "grid-1-3" ]
                    [ viewBreadCrumb model.viewMode
                    , viewDisbursements model.viewMode
                    ]
                ]

        ViewT Lawyers t ->
            div [ class "main" ]
                [ viewTop t
                , div [ class "grid-1-3" ]
                    [ viewBreadCrumb model.viewMode
                    , viewLawyers t Old
                    ]
                ]

        ViewT Property t ->
            div [ class "main" ]
                [ viewTop t
                , div [ class "grid-1-3" ]
                    [ viewBreadCrumb model.viewMode
                    , viewProperty t Old
                    ]
                ]

        ViewT Parties t ->
            div [ class "main" ]
                [ viewTop t
                , div [ class "grid-1-3" ]
                    [ viewBreadCrumb model.viewMode
                    , viewParties t Old
                    ]
                ]

        ViewNewT Lawyers t te ->
            div [ class "main" ]
                [ div [ class "grid-1-3" ]
                    [ viewBreadCrumb model.viewMode
                    , viewLawyers te New
                    ]
                ]

        ViewNewT Property t te ->
            div [ class "main" ]
                [ div [ class "grid-1-3" ] [ div [] [], h2 [] [ text "New Transaction" ] ]
                , div [ class "grid-1-3" ]
                    [ viewBreadCrumb model.viewMode
                    , viewProperty te New
                    ]
                ]

        ViewNewT Parties t te ->
            div [ class "main" ]
                [ div [ class "grid-1-3" ] [ div [] [], h2 [] [ text "New Transaction" ] ]
                , div [ class "grid-1-3" ]
                    [ viewBreadCrumb model.viewMode
                    , viewParties te New
                    ]
                ]

        ViewNewT _ t te ->
            div [ class "main" ]
                [ div [ class "grid-1-3" ] [ div [] [], h2 [] [ text "New Transaction" ] ]
                , div [ class "grid-1-3" ]
                    [ viewBreadCrumb model.viewMode
                    , viewReview model.viewMode
                    ]
                ]



-- *** NEW FUNCTIONS ***


viewTop : Transaction -> Html Msg
viewTop t =
    div [ id "tr-top" ]
        [ viewReturn
        , viewTransRes t
        ]


viewReturn : Html Msg
viewReturn =
    div [ id "tr-top-arrow" ]
        [ img
            [ onClick (SetViewMode ViewTransactionsList)
            , src "return.svg"
            , alt "return to all transactions"
            , width 50
            , height 90
            ]
            []
        ]


viewBreadCrumb : ViewMode -> Html Msg
viewBreadCrumb vm =
    case vm of
        ViewTransactionsList ->
            viewBCT Review

        ViewT Disbursement _ ->
            viewBCT Disbursement

        ViewT _ _ ->
            viewBCT Review

        ViewNewT Lawyers _ _ ->
            viewBCNewT Lawyers

        ViewNewT Property _ _ ->
            viewBCNewT Property

        ViewNewT Parties _ _ ->
            viewBCNewT Parties

        ViewNewT _ _ _ ->
            viewBCNewT Review


viewBCT : Stage -> Html Msg
viewBCT s =
    div [ class "tr-bread-crumb" ]
        [ p
            [ classList [ ( "bc-active", s /= Disbursement ) ]
            , onClick <| ChangeStage Review
            ]
            [ text "Transaction Details" ]
        , p
            [ classList [ ( "bc-active", s == Disbursement ) ]
            , onClick <| ChangeStage Disbursement
            ]
            [ text "Disbursements" ]
        ]


viewBCNewT : Stage -> Html Msg
viewBCNewT s =
    div [ class "tr-bread-crumb" ]
        [ p
            [ classList [ ( "bc-active", s == Lawyers ) ]
            ]
            [ text "Transaction Details" ]
        , p [ classList [ ( "bc-active", s == Property ) ] ] [ text "Property" ]
        , p [ classList [ ( "bc-active", s == Parties ) ] ] [ text "Parties" ]
        , p [ classList [ ( "bc-active", s == Review ) ] ] [ text "Review" ]
        ]



-- **** REVIEW ****
-- decided to use the same function for both new and existing transactions


viewReview : ViewMode -> Html Msg
viewReview vm =
    case vm of
        ViewTransactionsList ->
            div [] []

        ViewT s t ->
            div []
                [ h3 [] [ text "Overview" ]
                , viewReviewLawyer s t
                , viewReviewProperty s t
                , viewReviewParties s t
                ]

        ViewNewT s t te ->
            div [ class "tr-main" ]
                [ p [] [ text <| "Lawyer/Notary: " ++ "Keith Smith" ] ]


viewReviewLawyer : Stage -> Transaction -> Html Msg
viewReviewLawyer s t =
    div [ class "grid-1-1 border-gray" ]
        [ div []
            [ p [ class "gray padd-7" ] [ b [] [ text "Lawyer/Notary" ] ]
            , p [ class "padd-l-7 padd-t-7" ] [ text "Lawyer/Notary" ]
            , p [ class "padd-l-7" ] [ b [] [ text <| t.user.name ] ]
            , p [ class "padd-l-7 padd-t-7" ] [ text "Acting for" ]
            , p [ class "padd-l-7" ]
                [ b []
                    [ text <|
                        if t.transType == Sale then
                            "Vendor"

                        else
                            "Purchaser"
                    ]
                ]
            ]
        , div []
            [ p [ class "gray padd-7" ]
                [ b [] [ text "Other Lawyer/Notary" ]
                , button [ class "edit-review", onClick <| ChangeStage Lawyers ] [ text "Edit" ]
                ]
            , p [ class "padd-l-7 padd-t-7" ] [ text "Lawyer/Notary" ]
            , p [ class "padd-l-7" ] [ b [] [ text t.otherLawyer.lawyerName ] ]
            , p [ class "padd-l-7 padd-t-7" ] [ text "Acting for" ]
            , p [ class "padd-l-7" ]
                [ b []
                    [ text <|
                        if t.transType == Purchase then
                            "Vendor"

                        else
                            "Purchaser"
                    ]
                ]
            ]
        ]


viewReviewProperty : Stage -> Transaction -> Html Msg
viewReviewProperty s t =
    div [ class "grid-1-1 border-gray" ]
        [ div []
            [ p [ class "gray padd-7" ] [ b [] [ text "Transaction" ] ]
            , p [ class "padd-l-7 padd-t-7" ] [ text "Transaction Type" ]
            , p [ class "padd-l-7" ] [ b [] [ text <| transTypeToString t.transType ] ]
            , p [ class "padd-l-7 padd-t-7" ] [ text "Closing Date" ]
            , p [ class "padd-l-7" ] [ b [] [ text t.closing ] ]
            ]
        , div []
            [ p [ class "gray padd-7" ]
                [ b [] [ text "Property" ]
                , button [ class "edit-review", onClick <| ChangeStage Property ] [ text "Edit" ]
                ]
            , p [ class "padd-l-7 padd-t-7" ] [ text "Address" ]
            , p [ class "padd-l-7" ] [ b [] [ text t.property.street ] ]
            , p [ class "padd-l-7" ] [ b [] [ text <| t.property.city ++ ", " ++ t.property.postalCode ] ]
            ]
        ]


viewReviewParties : Stage -> Transaction -> Html Msg
viewReviewParties s t =
    div [ class "grid-1-1 border-gray" ]
        [ div []
            [ p [ class "gray padd-7" ]
                [ b []
                    [ text <|
                        if t.transType == Sale then
                            "Vendor"

                        else
                            "Purchaser"
                    ]
                ]
            , p [ class "padd-l-7 padd-t-7" ] [ text t.client ]
            ]
        , div []
            [ p [ class "gray padd-7" ]
                [ b []
                    [ text <|
                        if t.transType == Sale then
                            "Purchaser"

                        else
                            "Vendor"
                    ]
                , button [ class "edit-review", onClick <| ChangeStage Parties ] [ text "Edit" ]
                ]
            , p [ class "padd-l-7 padd-t-7" ] [ text t.otherClient ]
            ]
        ]



-- **** EDIT VIEWS ****


viewDisbursements : ViewMode -> Html Msg
viewDisbursements vm =
    div [ class "tr-main" ] [ h3 [] [ text "Manage Disbursements" ] ]


viewProperty : Transaction -> Mode -> Html Msg
viewProperty t mode =
    div [ class "tr-main" ]
        [ p [] [ h3 [] [ text "Property Details" ] ]
        , button [ onClick <| SetViewMode ViewTransactionsList ] [ text "Cancel" ]
        ]


viewLawyers : Transaction -> Mode -> Html Msg
viewLawyers t mode =
    div [ class "tr-main" ] [ h3 [] [ text "Legal Info" ] ]


viewParties : Transaction -> Mode -> Html Msg
viewParties t mode =
    div [ class "tr-main" ]
        [ h3 [] [ text "Purchaser / Seller" ]
        , label []
            [ text <|
                if t.transType == Purchase then
                    "Purchaser"

                else
                    "Vendor"
            ]
        , br [] []
        , input [ value t.client, onInput <| EditTransaction ClientF ] []
        , br [] []
        , label []
            [ text <|
                if t.transType == Purchase then
                    "Vendor"

                else
                    "Purchaser"
            ]
        , br [] []
        , input [ value t.otherClient, onInput <| EditTransaction OtherClientF ] []
        , div []
            [ button [ onClick DiscardEdits ] [ text "Cancel" ]
            , span [ class "spacing" ] []
            , button [ classList [ ( "hidden", mode == Old ) ], onClick SaveEditsAndContinue ] [ text "Save and Continue" ]
            , button [ classList [ ( "hidden", mode == New ) ], onClick SaveEdits ] [ text "Save changes" ]
            ]
        ]



-- *** OLD FUNCTIONS ***
--viewTransBreadCrumb : Transaction -> Html Msg
--viewTransBreadCrumb t =
--    div [ id "tr-bread-crumb" ]
--        [ h5 [] [ text "Transaction Details" ]
--        , h5 [] [ text "Disbursements" ]
--        ]


viewTransRes : Transaction -> Html msg
viewTransRes t =
    div [ id "tr-top-info" ]
        [ h3 [ style "color" "rgb(44,78,154)" ] [ text <| String.fromInt t.id ++ " - " ++ t.client ]
        , div [ class "grid-1-2" ]
            [ div []
                [ p [] [ text "Transaction type" ]
                , p [] [ text "Closing date:" ]
                , p [] [ text "Status" ]
                ]
            , div []
                [ p []
                    [ b [] [ text <| transTypeToString t.transType ] ]
                , p []
                    [ b [] [ text t.closing ] ]
                , p []
                    [ b [] [ text <| transStatusToString t.status ] ]
                ]
            ]
        ]


viewTransTopButtons : Html msg
viewTransTopButtons =
    div [ id "tr-top-buttons" ]
        [ p []
            [ button [] [ text "Delete Transaction" ]
            , button [] [ text "Disburse Funds" ]
            ]
        ]


viewTransactionDetails : Transaction -> Html Msg
viewTransactionDetails t =
    div []
        [ h3 [] [ text "Transaction Details" ]

        --, viewFieldDetails t
        , p [] [ text t.client ]

        --, p [] [ text t.property ]
        , p [] [ text t.closing ]
        , button [ onClick (SetViewMode ViewTransactionsList) ]
            [ text "Cancel" ]
        ]



--viewFieldDetails : TransField -> Transaction -> Html msg
--viewFieldDetails field t =
--    div [ class "grid-1-1" ]
--        [ div [ class "field-name" ] [ p [] [ text <| fieldToString field ] ]
--        , div [ class "field-value" ] [ p [] [ b [] [ text <| valueForField field t ] ] ]
--        ]
--viewTransactionModal : Maybe Transaction -> Html Msg
--viewTransactionModal mbt =
--    case mbt of
--        Nothing ->
--            div [ class "tr-detail", class "shifted" ]
--                [ a [ href "/transactions" ] [ text "Cancel" ]
--                ]
--        Just t ->
--            div [ class "tr-detail" ]
--                [ text <| String.fromInt t.id
--                -- navigate to transaction/:id
--                , button [ onClick (SetViewMode TransactionsList) ] [ text "Cancel" ]
--]


viewTransactionsSearch : Model -> Html Msg
viewTransactionsSearch model =
    div []
        [ div [ class "container-grid-search" ]
            [ div
                [ class "item-tr-search" ]
                [ h3 [] [ text " TRANSACTIONS PLATFORM" ] ]
            , div [ class "item-button" ]
                [ button [ onClick (SetViewMode <| ViewNewT Property (createNewTransaction model) (createNewTransaction model)) ] [ text "Create New Transaction" ]
                ]
            ]
        , div [ class "tr-search" ]
            [ h5 [ style "margin-top" "0" ] [ text "Search for a transaction by client, property, closing, etc." ]
            , i [ class "fa fa-search" ] []
            , input [ type_ "text", value model.searchField, onInput SearchFieldEdited ] []
            , button [ onClick ClearSearchField ] [ text "X" ]
            , h5 [] [ text "Show" ]
            , input
                [ type_ "radio"
                , name "transactions"
                , checked model.viewJMOTs
                , onClick ToggledJmot
                , style "cursor" "pointer"
                ]
                []
            , label [ style "margin-right" "27px" ] [ text "My Own" ]
            , input
                [ type_ "radio"
                , name "transactions"
                , checked (not <| model.viewJMOTs)
                , onClick ToggledJmot
                , style "cursor" "pointer"
                ]
                []
            , label [] [ text "My Organization" ]
            ]
        ]


createNewTransaction : Model -> Transaction
createNewTransaction model =
    { id = nextId model.transactions -- automatically give the next available id
    , user = sam
    , transType = Sale
    , client = ""
    , property = emptyAddress
    , closing = ""
    , status = Draft
    , otherLawyer = emptyLawyer
    , otherClient = "" -- the other person involved in transaction
    , disbursements = []
    }



-- want to create table column headers that do sorting
-- time for a new viewColHeader function


viewTransactionsTable : Model -> Html Msg
viewTransactionsTable tm =
    table []
        ([ colgroup []
            [ col [ style "width" "18%" ] []
            , col [ style "width" "22%" ] []
            , col [ style "width" "37%" ] []
            ]
         , tr [ class "tbl-header" ]
            [ viewColHeader TypeSort tm
            , viewColHeader ClientSort tm
            , viewColHeader PropertySort tm
            , viewColHeader ClosingSort tm
            , th [] []
            ]
         ]
            ++ (tm.transactions
                    |> filterWithFilters
                        -- multiple filtering based on viewTransactionSearch selections
                        -- add filtering by closed field ****
                        (boolUnzip
                            [ ( tm.viewJMOTs, belongsToId tm.loggedUserId )
                            , ( tm.searchField /= "", transactionContains tm.searchField )
                            ]
                        )
                    |> List.sortBy (sortFunction tm.transSort)
                    |> reverseIfSort tm.sortAscending
                    |> List.map viewTransactionLine
               )
        )


viewColHeader : TransSort -> Model -> Html Msg
viewColHeader tst tm =
    th [ onClick (SetTransSort tst) ]
        [ text <| transSortToString tst
        , i
            [ classList
                [ ( "fa fa-angle-up", not tm.sortAscending )
                , ( "fa fa-angle-down", tm.sortAscending )
                , ( "hidden", tst /= tm.transSort )
                ]
            ]
            []
        ]


viewTransactionLine : Transaction -> Html Msg
viewTransactionLine t =
    tr []
        [ td [] [ text <| transTypeToString t.transType ]
        , td [] [ text t.client ]
        , td [] [ text <| t.property.street ++ ", " ++ t.property.city ]
        , td [] [ text t.closing ]

        --, td [] [ span [ class "tr-list", onClick <| SetSelectedTrans t.id ] [ text "View" ] ]
        , td []
            [ span
                [ style "cursor" "pointer"
                , style "color" "#379"
                , onClick <| SetViewMode (ViewT Review t)
                ]
                [ text "View" ]
            ]
        ]



-- MAIN


subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
