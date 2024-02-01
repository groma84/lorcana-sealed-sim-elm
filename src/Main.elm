module Main exposing (main)

import Browser
import Html exposing (Html, div, img)
import Html.Attributes exposing (src, style)
import Json.Decode as D
import VitePluginHelper
import Html exposing (text)
import Html exposing (button)
import Html.Events exposing (onClick)


type alias Model =
    { allCards : List Card
       , error : Maybe String
    }


initialModel =
    { allCards = []
    , error = Nothing
    }


type alias CharacterCard =
    { abilities : Maybe String
    , artist : String
    , bodyText : Maybe String
    , cardNum : Int
    , classifications : Maybe String
    , color : String
    , cost : Int
    , enchantedImage : Maybe String
    , flavorText : Maybe String
    , foilImage : Maybe String
    , image : String
    , inkable : Bool
    , lore : Int
    , name : String
    , rarity : Rarity
    , setID : String
    , setName : String
    , setNum : Int
    , strength : Int
    , type_ : String
    , willpower : Int
    }


type alias ActionCard =
    { artist : String
    , bodyText : Maybe String
    , cardNum : Int
    , color : String
    , cost : Int
    , enchantedImage : Maybe String
    , flavorText : Maybe String
    , foilImage : Maybe String
    , image : String
    , inkable : Bool
    , name : String
    , rarity : Rarity
    , setID : String
    , setName : String
    , setNum : Int
    , type_ : String
    }


type alias SongCard =
    { artist : String
    , bodyText : Maybe String
    , cardNum : Int
    , color : String
    , cost : Int
    , enchantedImage : Maybe String
    , flavorText : Maybe String
    , foilImage : Maybe String
    , image : String
    , inkable : Bool
    , name : String
    , rarity : Rarity
    , setID : String
    , setName : String
    , setNum : Int
    , type_ : String
    }


type alias ItemCard =
    { artist : String
    , bodyText : Maybe String
    , cardNum : Int
    , color : String
    , cost : Int
    , enchantedImage : Maybe String
    , flavorText : Maybe String
    , foilImage : Maybe String
    , image : String
    , inkable : Bool
    , name : String
    , rarity : Rarity
    , setID : String
    , setName : String
    , setNum : Int
    , type_ : String
    }


type Card
    = Character CharacterCard
    | Action ActionCard
    | Song SongCard
    | Item ItemCard
    | Failed


type Msg
    = NoOp
    | GeneratePacks

type Rarity = Common | Uncommon | Rare | SuperRare | Legendary

decodeRarity : String -> D.Decoder Rarity
decodeRarity rarityText =
    case rarityText of
        "Common" -> D.succeed Common
        "Uncommon" -> D.succeed Uncommon
        "Rare" -> D.succeed Rare
        "Super Rare" -> D.succeed SuperRare
        "Legendary" -> D.succeed Legendary
        _ -> D.fail "Rarity could not be decoded"



classifyAndDecodeCard : D.Decoder Card
classifyAndDecodeCard =
    D.field "Type" D.string
        |> D.andThen decodeCard


decodeCharacterCard : D.Decoder Card
decodeCharacterCard =
    let
        fieldSet0 =
            D.map8 CharacterCard
                (D.maybe (D.field "Abilities" D.string))
                (D.field "Artist" D.string)
                (D.maybe (D.field "Body_Text" D.string))
                (D.field "Card_Num" D.int)
                (D.maybe (D.field "Classifications" D.string))
                (D.field "Color" D.string)
                (D.field "Cost" D.int)
                (D.maybe (D.field "Enchanted_Image" D.string))

        fieldSet1 =
            D.map8 (<|)
                fieldSet0
                (D.maybe (D.field "Flavor_Text" D.string))
                (D.maybe (D.field "Foil_Image" D.string))
                (D.field "Image" D.string)
                (D.field "Inkable" D.bool)
                (D.field "Lore" D.int)
                (D.field "Name" D.string)
                (D.field "Rarity" D.string |> D.andThen decodeRarity)
    in
    D.map7 (<|)
        fieldSet1
        (D.field "Set_ID" D.string)
        (D.field "Set_Name" D.string)
        (D.field "Set_Num" D.int)
        (D.field "Strength" D.int)
        (D.field "Type" D.string)
        (D.field "Willpower" D.int)
    |> D.map Character


decodeSongCard : D.Decoder Card
decodeSongCard =
    let
        fieldSet0 =
            D.map8 SongCard
                (D.field "Artist" D.string)
                (D.maybe (D.field "Body_Text" D.string))
                (D.field "Card_Num" D.int)
                (D.field "Color" D.string)
                (D.field "Cost" D.int)
                (D.maybe (D.field "Enchanted_Image" D.string))
                (D.maybe (D.field "Flavor_Text" D.string))
                (D.maybe (D.field "Foil_Image" D.string))

        fieldSet1 =
            D.map8 (<|)
                fieldSet0
                (D.field "Image" D.string)
                (D.field "Inkable" D.bool)
                (D.field "Name" D.string)
                (D.field "Rarity" D.string |> D.andThen decodeRarity)
                (D.field "Set_ID" D.string)
                (D.field "Set_Name" D.string)
                (D.field "Set_Num" D.int)
    in
    D.map2 (<|)
        fieldSet1
        (D.field "Type" D.string)
    |> D.map Song


decodeActionCard : D.Decoder Card
decodeActionCard =
    let
        fieldSet0 =
            D.map8 ActionCard
                (D.field "Artist" D.string)
                (D.maybe (D.field "Body_Text" D.string))
                (D.field "Card_Num" D.int)
                (D.field "Color" D.string)
                (D.field "Cost" D.int)
                (D.maybe (D.field "Enchanted_Image" D.string))
                (D.maybe (D.field "Flavor_Text" D.string))
                (D.maybe (D.field "Foil_Image" D.string))

        fieldSet1 =
            D.map8 (<|)
                fieldSet0
                (D.field "Image" D.string)
                (D.field "Inkable" D.bool)
                (D.field "Name" D.string)
                (D.field "Rarity" D.string |> D.andThen decodeRarity)
                (D.field "Set_ID" D.string)
                (D.field "Set_Name" D.string)
                (D.field "Set_Num" D.int)
    in
    D.map2 (<|)
        fieldSet1
        (D.field "Type" D.string)
    |> D.map Action


decodeItemCard : D.Decoder Card
decodeItemCard =
    let
        fieldSet0 =
            D.map8 ItemCard
                (D.field "Artist" D.string)
                (D.maybe (D.field "Body_Text" D.string))
                (D.field "Card_Num" D.int)
                (D.field "Color" D.string)
                (D.field "Cost" D.int)
                (D.maybe (D.field "Enchanted_Image" D.string))
                (D.maybe (D.field "Flavor_Text" D.string))
                (D.maybe (D.field "Foil_Image" D.string))

        fieldSet1 =
            D.map8 (<|)
                fieldSet0
                (D.field "Image" D.string)
                (D.field "Inkable" D.bool)
                (D.field "Name" D.string)
                (D.field "Rarity" D.string |> D.andThen decodeRarity)
                (D.field "Set_ID" D.string)
                (D.field "Set_Name" D.string)
                (D.field "Set_Num" D.int)
    in
    D.map2 (<|)
        fieldSet1
        (D.field "Type" D.string)
    |> D.map Item


decodeCard : String -> D.Decoder Card
decodeCard cardType =
    case cardType of
        "Character" ->
            decodeCharacterCard

        "Action - Song" ->
            decodeSongCard

        "Action" ->
            decodeActionCard

        "Item" ->
            decodeItemCard

        _ -> D.fail ("cardType " ++ cardType ++ " failed to decode")


main : Program D.Value Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }




init : D.Value -> ( Model, Cmd Msg )
init flags =
    let
        parsedCards =
            D.decodeValue (D.list classifyAndDecodeCard) flags

        newModel =
            case parsedCards of
                Err e ->
                    { error = Just (D.errorToString e)
                    , allCards = []
                    }

                Ok cards ->
                    { error = Nothing
                    , allCards = cards
                    }
    in
    ( newModel
    , Cmd.none
    )

type alias CardsSplitForPack = 
    {
        commons: List Card
        , uncommons: List Card
        , rares: List Card
        , superRares : List Card
        , legendaries : List Card
        , allCards: List Card
    }


prepareCardLists : List Card -> CardsSplitForPack
prepareCardLists allCards =
    let 
        rarityFromCard card = 
            case card of
                Character x -> x.rarity
                Song x -> x.rarity
                Action x -> x.rarity
                Item x -> x.rarity
                Failed -> Common
    in
    {
        allCards = allCards
        , commons = List.filter (\c -> rarityFromCard c == Common) allCards
        , uncommons = List.filter (\c -> rarityFromCard c == Uncommon) allCards
        , rares = List.filter (\c -> rarityFromCard c == Rare) allCards
        , superRares = List.filter (\c -> rarityFromCard c == SuperRare) allCards
        , legendaries = List.filter (\c -> rarityFromCard c == Legendary) allCards
    }

-- TODO: allCards -> prepareCardLists -> Shuffle Each List and "store" again + get three more random nums for the distributions
-- -> use generated values into real generator function -> filter for pack set -> generate pack -> loop for packs -> store everything
-- -> display everything

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
        GeneratePacks ->
            (model, Cmd.none)





generatePacksSelection : Model -> Html Msg
generatePacksSelection model =
    button [onClick GeneratePacks] [text "Generate 3 TFC and 3 RotF packs"]

view : Model -> Html Msg
view model =
    div []
        [
            case model.error of
                Nothing -> generatePacksSelection model
                Just e -> div [] [text e]
            
        ]
