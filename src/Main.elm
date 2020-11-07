module Main exposing (main)

--import Static

import Browser
import Html exposing (Html, a, b, br, button, col, colgroup, div, em, h3, h5, hr, i, img, input, label, li, p, span, table, td, text, th, tr, ul)
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


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
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
    | ViewNewT Stage NewTransaction


changeViewStage : Stage -> ViewMode -> ViewMode
changeViewStage s vm =
    case vm of
        ViewTransactionsList ->
            ViewTransactionsList

        ViewT _ t ->
            ViewT s t

        ViewNewT _ nt ->
            ViewNewT s nt


type Stage
    = Review
    | Property
    | Lawyers
    | Parties
    | Disbursement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeStage s ->
            ( { model | viewMode = changeViewStage s model.viewMode }, Cmd.none )

        AdjustTimeZone z ->
            ( { model | zone = z }, Cmd.none )

        SetViewMode vm ->
            ( { model | viewMode = vm }, Cmd.none )

        --SetSelectedTrans id ->
        --    ( { model | selTransaction = selectTransaction id model.transactions }, Cmd.none )
        --ClearSelectedTransaction ->
        --    ( { model | selTransaction = Nothing }, Cmd.none )
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
                    , viewReview model
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
                    , viewLawyers model.viewMode
                    ]
                ]

        ViewT Property t ->
            div [ class "main" ]
                [ viewTop t
                , div [ class "grid-1-3" ]
                    [ viewBreadCrumb model.viewMode
                    , viewProperty model.viewMode
                    ]
                ]

        ViewT Parties t ->
            div [ class "main" ]
                [ viewTop t
                , div [ class "grid-1-3" ]
                    [ viewBreadCrumb model.viewMode
                    , viewParties model.viewMode
                    ]
                ]

        ViewNewT Lawyers nt ->
            div [ class "main" ]
                [ div [ class "grid-1-3" ]
                    [ viewBreadCrumb model.viewMode
                    , viewLawyers model.viewMode
                    ]
                ]

        ViewNewT Property nt ->
            div [ class "main" ]
                [ div [ class "grid-1-3" ]
                    [ viewBreadCrumb model.viewMode
                    , viewProperty model.viewMode
                    ]
                ]

        ViewNewT Parties nt ->
            div [ class "main" ]
                [ div [ class "grid-1-3" ]
                    [ viewBreadCrumb model.viewMode
                    , viewParties model.viewMode
                    ]
                ]

        ViewNewT _ nt ->
            div [ class "main" ]
                [ div [ class "grid-1-3" ]
                    [ viewBreadCrumb model.viewMode
                    , viewReview model
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

        ViewNewT Lawyers _ ->
            viewBCNewT Lawyers

        ViewNewT Property _ ->
            viewBCNewT Property

        ViewNewT Parties _ ->
            viewBCNewT Parties

        ViewNewT _ _ ->
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


viewReview : Model -> Html Msg
viewReview model =
    case model.viewMode of
        ViewTransactionsList ->
            div [] []

        ViewT s t ->
            div []
                [ h3 [] [ text "Transaction Details" ]
                , div [ class "grid-1-1 border-gray" ]
                    [ div []
                        [ p [ class "gray padd-7" ] [ b [] [ text "Lawyer/Notary" ] ]
                        , p [ class "padd-l-7 padd-t-7" ] [ text "Lawyer/Notary" ]
                        , p [ class "padd-l-7" ] [ b [] [ text <| String.fromInt t.id ] ]
                        , p [ class "padd-l-7 padd-t-7" ] [ text "Acting for" ]
                        , p [ class "padd-l-7" ] [ b [] [ text "Purchaser" ] ]
                        ]
                    , div []
                        [ p [ class "gray padd-7" ] [ b [] [ text "Other Lawyer/Notary" ] ]
                        , p [ class "padd-l-7 padd-t-7" ] [ text "Lawyer/Notary" ]
                        , p [ class "padd-l-7" ] [ b [] [ text <| String.fromInt t.id ] ]
                        , p [ class "padd-l-7 padd-t-7" ] [ text "Acting for" ]
                        , p [ class "padd-l-7" ] [ b [] [ text "Vendor" ] ]
                        ]
                    ]
                , div [ class "grid-1-1 border-gray" ]
                    [ div []
                        [ p [ class "gray padd-7" ] [ b [] [ text "Transaction" ] ]
                        , p [ class "padd-l-7 padd-t-7" ] [ text "Transaction Type" ]
                        , p [ class "padd-l-7" ] [ b [] [ text <| transTypeToString t.transType ] ]
                        , p [ class "padd-l-7 padd-t-7" ] [ text "Closing Date" ]
                        , p [ class "padd-l-7" ] [ b [] [ text t.closing ] ]
                        ]
                    , div []
                        [ p [ class "gray padd-7" ] [ b [] [ text "Property" ] ]
                        , p [ class "padd-l-7 padd-t-7" ] [ text "Address" ]
                        , p [ class "padd-l-7" ] [ b [] [ text t.property ] ]
                        , p [ class "padd-l-7 padd-t-7" ] [ text "Address" ]
                        , p [ class "padd-l-7" ] [ b [] [ text t.property ] ]
                        ]
                    ]
                , div [ class "grid-1-1 border-gray" ]
                    [ div []
                        [ p [ class "gray padd-7" ] [ b [] [ text "Purchaser" ] ]
                        , p [ class "padd-l-7 padd-t-7" ] [ text "Lawyer/Notary" ]
                        , p [ class "padd-l-7" ] [ b [] [ text <| String.fromInt t.id ] ]
                        , p [ class "padd-l-7 padd-t-7" ] [ text "Acting for" ]
                        , p [ class "padd-l-7" ] [ b [] [ text "Purchaser" ] ]
                        ]
                    , div []
                        [ p [ class "gray padd-7" ] [ b [] [ text "Vendor" ] ]
                        , p [ class "padd-l-7 padd-t-7" ] [ text "Lawyer/Notary" ]
                        , p [ class "padd-l-7" ] [ b [] [ text <| String.fromInt t.id ] ]
                        , p [ class "padd-l-7 padd-t-7" ] [ text "Acting for" ]
                        , p [ class "padd-l-7" ] [ b [] [ text "Vendor" ] ]
                        ]
                    ]
                ]

        ViewNewT s nt ->
            div [ class "tr-main" ]
                [ p [] [ text <| "Lawyer/Notary: " ++ "Keith Smith" ] ]


viewReviewLawyer : Stage -> Transaction -> Html Msg
viewReviewLawyer s t =
    div [ class "grid-1-1 border-gray" ]
        [ div []
            [ p [ class "gray padd-7" ] [ b [] [ text "Lawyer/Notary" ] ]
            , p [ class "padd-l-7 padd-t-7" ] [ text "Lawyer/Notary" ]
            , p [ class "padd-l-7" ] [ b [] [ text <| String.fromInt t.id ] ]
            , p [ class "padd-l-7 padd-t-7" ] [ text "Acting for" ]
            , p [ class "padd-l-7" ] [ b [] [ text "Purchaser" ] ]
            ]
        , div []
            [ p [ class "gray padd-7" ] [ b [] [ text "Other Lawyer/Notary" ] ]
            , p [ class "padd-l-7 padd-t-7" ] [ text "Lawyer/Notary" ]
            , p [ class "padd-l-7" ] [ b [] [ text <| String.fromInt t.id ] ]
            , p [ class "padd-l-7 padd-t-7" ] [ text "Acting for" ]
            , p [ class "padd-l-7" ] [ b [] [ text "Vendor" ] ]
            ]
        ]


viewDisbursements : ViewMode -> Html Msg
viewDisbursements vm =
    div [ class "tr-main" ] [ text "DISBURSEMENTS EDIT" ]


viewProperty : ViewMode -> Html Msg
viewProperty vm =
    div [ class "tr-main" ] [ text "EDIT ADDRESS INFO" ]


viewLawyers : ViewMode -> Html Msg
viewLawyers vm =
    div [ class "tr-main" ] [ text "EDIT BOTH PARTIES LAWYERS" ]


viewParties : ViewMode -> Html Msg
viewParties vm =
    div [ class "tr-main" ] [ text "EDIT PURCHASER / SELLER" ]



-- *** OLD FUCNTIONS ***
--viewTransaction : Transaction -> Html Msg
--viewTransaction t =
--    div []
--        [ div [ id "tr-top" ]
--            [ viewReturn
--            , viewTransRes t
--            , viewTransTopButtons
--            ]
--        , div [ id "tr-bottom" ]
--            [ viewTransBreadCrumb t
--            , div [ id "tr-bottom-main" ]
--                [ viewTransactionDetails t
--                ]
--            ]
--        ]


viewTransBreadCrumb : Transaction -> Html Msg
viewTransBreadCrumb t =
    div [ id "tr-bread-crumb" ]
        [ h5 [] [ text "Transaction Details" ]
        , h5 [] [ text "Disbursements" ]
        ]


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
        , p [] [ text t.property ]
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
                [ button [] [ text "Create New Transaction" ]
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

            --, hr [] []
            --, text <| boolToString model.viewJMOTs
            ]
        ]



-- want to create table column headers that do sorting
-- time for a new viewColHeader function


viewTransactionsTable : Model -> Html Msg
viewTransactionsTable tm =
    table []
        ([ colgroup []
            [ col [ style "width" "18%" ] []
            , col [ style "width" "22%" ] []
            , col [ style "width" "33%" ] []
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
        , td [] [ text t.property ]
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
