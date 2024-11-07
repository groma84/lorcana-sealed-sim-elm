module Main exposing (main)

import Browser
import Html exposing (Html, button, div, img, text, li, input)
import Html.Attributes exposing (src, style, type_, min, max)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import VitePluginHelper
import Random.List
import List.Extra
import Dict
import Random
import Random.Extra
import Maybe.Extra
import AllDict
import Platform.Cmd as Cmd


type alias Model =
    { allCards : List Card
    , error : Maybe String
    , generatedBooster : (List (Maybe Booster))
    , packsToGenerate : Dict.Dict Int Int
    }

initialModel =
    { allCards = []
    , error = Nothing
    , generatedBooster = []
    , packsToGenerate = Dict.empty
    }


type alias CharacterCard =
    { abilities : Maybe String
    , artist : String
    , bodyText : Maybe String
    , cardNum : Int
    , classifications : Maybe String
    , color : Color
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
    , color : Color
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
    , color : Color
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
    , color : Color
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

type alias Booster = 
    { commonSteel : Card
    , commonRuby : Card
    , commonAmber : Card
    , commonSapphire : Card
    , commonEmerald : Card
    , commonAmethyst : Card
    , uncommon1 : Card
    , uncommon2 : Card
    , uncommon3 : Card
    , rare1: Card
    , rare2: Card
    , foil: Card
    }

type alias MaybeBooster = 
    { commonSteel : Maybe Card
    , commonRuby : Maybe Card
    , commonAmber : Maybe Card
    , commonSapphire : Maybe Card
    , commonEmerald : Maybe Card
    , commonAmethyst : Maybe Card
    , uncommons : Maybe (Card, Card, Card)
    , rares: Maybe (Card, Card)
    , foil: Maybe Card
    }    

type Msg
    = NoOp
    | GeneratePacks
    | BoosterGenerated (List (Maybe Booster))
    | PackSelectionChanged Int String


type Rarity
    = Common
    | Uncommon
    | Rare
    | SuperRare
    | Legendary


type Color
    = Steel
    | Ruby
    | Amber
    | Sapphire
    | Emerald
    | Amethyst


decodeRarity : String -> D.Decoder Rarity
decodeRarity rarityText =
    case rarityText of
        "Common" ->
            D.succeed Common

        "Uncommon" ->
            D.succeed Uncommon

        "Rare" ->
            D.succeed Rare

        "Super Rare" ->
            D.succeed SuperRare

        "Legendary" ->
            D.succeed Legendary

        _ ->
            D.fail ("Rarity '" ++ rarityText ++ "' could not be decoded")


decodeColor : String -> D.Decoder Color
decodeColor colorText =
    case colorText of
        "Steel" ->
            D.succeed Steel

        "Ruby" ->
            D.succeed Ruby

        "Amber" ->
            D.succeed Amber

        "Sapphire" ->
            D.succeed Sapphire

        "Emerald" ->
            D.succeed Emerald

        "Amethyst" ->
            D.succeed Amethyst

        _ ->
            D.fail ("Color '" ++ colorText ++ "' could not be decoded")


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
                (D.field "Color" D.string |> D.andThen decodeColor)
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
                (D.field "Color" D.string |> D.andThen decodeColor)
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
                (D.field "Color" D.string |> D.andThen decodeColor)
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
                (D.field "Color" D.string |> D.andThen decodeColor)
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

        _ ->
            D.fail ("cardType " ++ cardType ++ " failed to decode")


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
                    { initialModel | error = Just (D.errorToString e)}

                Ok cards ->
                    { initialModel | allCards = cards}
    in
    ( newModel
    , Cmd.none
    )


type alias CardsSplitForPack =
    { commons : List Card
    , uncommons : List Card
    , rares : List Card
    , superRares : List Card
    , legendaries : List Card
    , allCards : List Card
    }


colorFromCard : Card -> Color
colorFromCard card =
    case card of
        Character x ->
            x.color

        Song x ->
            x.color

        Action x ->
            x.color

        Item x ->
            x.color

        Failed ->
            Steel

rarityFromCard : Card -> Rarity
rarityFromCard card =
            case card of
                Character x ->
                    x.rarity

                Song x ->
                    x.rarity

                Action x ->
                    x.rarity

                Item x ->
                    x.rarity

                Failed ->
                    Common

setNumberFromCard : Card -> Int
setNumberFromCard card =
            case card of
                Character x ->
                    x.setNum

                Song x ->
                    x.setNum

                Action x ->
                    x.setNum

                Item x ->
                    x.setNum

                Failed ->
                    -1

prepareCardLists : List Card -> Int -> CardsSplitForPack
prepareCardLists allCards setNumber =
    { allCards = List.filter (\c -> setNumberFromCard c == setNumber ) allCards
    , commons = List.filter (\c -> rarityFromCard c == Common && setNumberFromCard c == setNumber) allCards
    , uncommons = List.filter (\c -> rarityFromCard c == Uncommon && setNumberFromCard c == setNumber) allCards
    , rares = List.filter (\c -> rarityFromCard c == Rare && setNumberFromCard c == setNumber) allCards
    , superRares = List.filter (\c -> rarityFromCard c == SuperRare && setNumberFromCard c == setNumber) allCards
    , legendaries = List.filter (\c -> rarityFromCard c == Legendary && setNumberFromCard c == setNumber) allCards
    }


generateOneCard : Maybe (List Card) -> Random.Generator (Maybe Card)
generateOneCard possibleCards =
    case possibleCards of
            Nothing -> Random.constant Nothing
            Just cards -> Random.Extra.sample cards

generateUncommons: List Card -> Random.Generator (Maybe (Card, Card, Card))
generateUncommons possibleUncommons =
    case possibleUncommons of
        [] -> Random.constant Nothing    
        cards -> cards  
                        |> Random.List.choices 3  
                        |> Random.map (\lists -> 
                                case lists of
                                    ([a, b, c], _) -> Just (a, b, c)
                                    _ -> Nothing  
                            )

generateRares: List Card -> Random.Generator (Maybe (Card, Card))
generateRares possibleRares =
    case possibleRares of
        [] -> Random.constant Nothing    
        cards -> cards  
                        |> Random.List.choices 2  
                        |> Random.map (\lists -> 
                                case lists of
                                    ([a, b], _) -> Just (a, b)
                                    _ -> Nothing  
                            )

generateBooster : CardsSplitForPack -> Random.Generator (Maybe Booster)
generateBooster cardsSplitForPack =
    let
        generateMaybeBooster : Random.Generator MaybeBooster
        generateMaybeBooster =
            let
                commonsGroupedByColor =
                    cardsSplitForPack.commons
                    |> List.Extra.gatherWith (\l r -> (colorFromCard l) == (colorFromCard r)) 
                    |> List.map (Tuple.mapFirst colorFromCard)
                    |> AllDict.fromList
                
            in
            -- Commons are one per color always
            Random.map MaybeBooster (generateOneCard (AllDict.get Steel commonsGroupedByColor))
            |> Random.Extra.andMap (generateOneCard (AllDict.get Ruby commonsGroupedByColor))
            |> Random.Extra.andMap (generateOneCard (AllDict.get Amber commonsGroupedByColor))
            |> Random.Extra.andMap (generateOneCard (AllDict.get Sapphire commonsGroupedByColor))
            |> Random.Extra.andMap (generateOneCard (AllDict.get Emerald commonsGroupedByColor))
            |> Random.Extra.andMap (generateOneCard (AllDict.get Amethyst commonsGroupedByColor))

            |> Random.Extra.andMap (generateUncommons cardsSplitForPack.uncommons)
            |> Random.Extra.andMap (generateRares (cardsSplitForPack.rares ++ cardsSplitForPack.superRares ++ cardsSplitForPack.legendaries))
            |> Random.Extra.andMap (generateOneCard (Just cardsSplitForPack.allCards))
    in
        generateMaybeBooster
        |> Random.map (\mb -> 
            let

                uncommonsAsList = case mb.uncommons of
                    Nothing -> [Nothing]
                    Just (c1, c2, c3) -> [Just c1, Just c2, Just c3]
                raresAsList = case mb.rares of
                    Nothing -> [Nothing]
                    Just (c1, c2) -> [Just c1, Just c2]                
                asList = [mb.commonSteel, mb.commonRuby, mb.commonAmber, mb.commonSapphire, mb.commonEmerald, mb.commonAmethyst] ++ uncommonsAsList ++ raresAsList ++ [mb.foil]
                traversed = Maybe.Extra.combine asList
            in
                case traversed of
                    Just [commonSteel, commonRuby, commonAmber, commonSapphire, commonEmerald, commonAmethyst, uc1, uc2, uc3, r1, r2, foil] ->
                        Just (Booster commonSteel commonRuby commonAmber commonSapphire commonEmerald commonAmethyst uc1 uc2 uc3 r1 r2 foil)
                    _ -> Nothing
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GeneratePacks ->
            let
                boosterGenerators = 
                    Dict.toList model.packsToGenerate
                    |> List.concatMap (\(setNo, amount) -> List.repeat amount setNo)
                    |> Random.Extra.traverse (\setNo -> prepareCardLists model.allCards setNo |> generateBooster)
            in
            ( model, Random.generate BoosterGenerated boosterGenerators)

        BoosterGenerated booster ->
            ( {model | generatedBooster = booster}, Cmd.none )
        
        PackSelectionChanged setNo val ->
            -- TODO
            ( model, Cmd.none )




generatePacksSelection : Model -> List (Html Msg)
generatePacksSelection model =
    -- TODO: Enable pack amount and set selection
    let
        oneSet setNo setName = li [] [
            input [type_ "number", Html.Attributes.min "0", Html.Attributes.max "6", onInput (PackSelectionChanged setNo)] []
            , text setName
            ]
    in
    [
        oneSet 1 "The First Chapter"
        , button [ onClick GeneratePacks ] [ text "Generate some packs" ]
    ]


view : Model -> Html Msg
view model =
    div []
        (case model.error of
            Nothing ->
                generatePacksSelection model

            Just e ->
                [div [] [ text e ]]

                -- TODO: implement generated pack view
        )