module Main exposing (main)

import Browser
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
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
    , cardsSplitForPack : Maybe CardsSplitForPack
    , error : Maybe String
    , maybeGeneratedBooster : Maybe MaybeBooster
    , generatedBooster : Maybe Booster
    }


initialModel =
    { allCards = []
    , cardsSplitForPack = Nothing
    , error = Nothing
    , maybeGeneratedBooster = Nothing
    , generatedBooster = Nothing
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
    | MaybeBoosterGenerated MaybeBooster


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
                    { error = Just (D.errorToString e)
                    , cardsSplitForPack = Nothing 
                    , allCards = []
                    , maybeGeneratedBooster = Nothing
                    , generatedBooster = Nothing
                    }

                Ok cards ->
                    { error = Nothing
                    , cardsSplitForPack = Just (prepareCardLists cards)
                    , allCards = cards
                    , maybeGeneratedBooster = Nothing
                    , generatedBooster = Nothing
                    }
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

prepareCardLists : List Card -> CardsSplitForPack
prepareCardLists allCards =
    { allCards = allCards
    , commons = List.filter (\c -> rarityFromCard c == Common) allCards
    , uncommons = List.filter (\c -> rarityFromCard c == Uncommon) allCards
    , rares = List.filter (\c -> rarityFromCard c == Rare) allCards
    , superRares = List.filter (\c -> rarityFromCard c == SuperRare) allCards
    , legendaries = List.filter (\c -> rarityFromCard c == Legendary) allCards
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

generateGenerators : CardsSplitForPack -> Maybe (Random.Generator Booster)
generateGenerators cardsSplitForPack =
    let
        commonsGroupedByColor =
            cardsSplitForPack.commons
            |> List.Extra.gatherWith (\l r -> (colorFromCard l) == (colorFromCard r)) 
            |> List.map (Tuple.mapFirst colorFromCard)
            |> AllDict.fromList

        everyColorHasAtLeastOneCard =
            (Maybe.Extra.isJust (AllDict.get Steel commonsGroupedByColor))
            && (Maybe.Extra.isJust (AllDict.get Ruby commonsGroupedByColor))
            && (Maybe.Extra.isJust (AllDict.get Amber commonsGroupedByColor))
            && (Maybe.Extra.isJust (AllDict.get Sapphire commonsGroupedByColor))
            && (Maybe.Extra.isJust (AllDict.get Emerald commonsGroupedByColor))
            && (Maybe.Extra.isJust (AllDict.get Amethyst commonsGroupedByColor))

        enoughUncommonsExist = List.length cardsSplitForPack.uncommons >= 3
        enoughRaresExist = (List.length cardsSplitForPack.rares + List.length cardsSplitForPack.superRares + List.length cardsSplitForPack.legendaries) >= 2

    in
        if everyColorHasAtLeastOneCard && enoughUncommonsExist && enoughRaresExist then
            -- TODO 2024-10-17: build one big generator with all the values that are guaranteed to exist
            --      AND create a new record to store the values/tuples to later map to Booster
            Just ()
        else
            Nothing

generateBooster : CardsSplitForPack -> Random.Generator (Maybe Booster)
generateBooster cardsSplitForPack =
    cardsSplitForPack
    |> generateMaybeBooster 
    |> Random.map (\mb -> 
        let
            uncommonsAsList = case mb.uncommons of
                Nothing -> [Nothing]
                Just (c1, c2, c3) -> [Just c1, Just c2, Just c3]
            asList = [mb.commonSteel, mb.commonRuby, mb.commonAmber, mb.commonSapphire, mb.commonEmerald, mb.commonAmethyst] ++ uncommonsAsList
            traversed = Maybe.Extra.combine asList
        in
            case traversed of
                Just [commonSteel, commonRuby, commonAmber, commonSapphire, commonEmerald, commonAmethyst, uc1, uc2, uc3] ->
                   Just (Booster commonSteel commonRuby commonAmber commonSapphire commonEmerald commonAmethyst uc1 uc2 uc3 commonSteel commonSteel commonSteel)
                _ -> Nothing

    )


generateMaybeBooster : CardsSplitForPack -> Random.Generator MaybeBooster
generateMaybeBooster cardsSplitForPack =
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



-- TODO: allCards -> prepareCardLists -> Shuffle Each List and "store" again + get three more random nums for the distributions
-- -> use generated values into real generator function -> filter for pack set -> generate pack -> loop for packs -> store everything
-- -> display everything
-- 6 commons (**one of each color!**), 3 uncommons, 1 mit rare oder superrare oder legendary, 1 mit rare oder superrare oder legendary, 1 mit foil mit beliebiger rarity


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        -- TODO: generate multiple packs
        GeneratePacks ->
            ( model, model.cardsSplitForPack
                        |> Maybe.map generateBooster
                        |> Maybe.map (Random.generate MaybeBoosterGenerated)
                        |> Maybe.withDefault Cmd.none)

        MaybeBoosterGenerated mb ->
            let
                booster = (Just Booster)
                    |> Maybe.Extra.andMap mb.commonSteel
                    |> Maybe.Extra.andMap mb.commonRuby
                    |> Maybe.Extra.andMap mb.commonAmber
                    |> Maybe.Extra.andMap mb.commonSapphire
                    |> Maybe.Extra.andMap mb.commonEmerald
                    |> Maybe.Extra.andMap mb.commonAmethyst
                    |> Maybe.Extra.andMap mb.uncommon1
                    |> Maybe.Extra.andMap mb.uncommon2
                    |> Maybe.Extra.andMap mb.uncommon3
                    |> Maybe.Extra.andMap mb.rare1 
                    |> Maybe.Extra.andMap mb.rare2
                    |> Maybe.Extra.andMap mb.foil
            in
            ( {model | generatedBooster = booster}, Cmd.none )



generatePacksSelection : Model -> Html Msg
generatePacksSelection model =
    -- TODO: Enable pack number and set selection
    button [ onClick GeneratePacks ] [ text "Generate some packs" ]


view : Model -> Html Msg
view model =
    div []
        [ case model.error of
            Nothing ->
                generatePacksSelection model

            Just e ->
                div [] [ text e ]
        ]
