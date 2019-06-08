import Language.JSON

record Class where
    constructor ClassDefn
    id : String
    division : String
    schedule : List( String, String )

parseClasses : String -> List Class
parseClasses str = case parse str of -- Maybe JSON
    Nothing => []
    Just( JArray vals ) => mapMaybe mkClass vals
  where
    mkClass : JSON -> Maybe Class
    mkClass( JObject fields ) = do
      JString id <- lookup "id" fields -- Maybe JSON
      JString division <- lookup "division" fields
      -- schedule <- lookup "schedule" fields
      pure( ClassDefn id division ?schedule )

