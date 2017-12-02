# home-run-wannabe

a Haskell wannabe version of my PureScript library: https://github.com/justinwoo/purescript-home-run-ball

features Csongor's GRowToList and ugly concrete record data types, so you can't do as cool things as my PureScript library (like composing a bunch of validation rows together)

should still be fun to look at and consider using. if there's ever demand, could be published as a library.

tl;dr

```hs
-- some validations we care about
data BeginsWith (s :: Symbol)
data Contains (s :: Symbol)
data EndsWith (s :: Symbol)

instance KnownSymbol prefix => ValidateRule (BeginsWith prefix) String where
  validateRuleImpl _ input = symbolVal (Proxy @prefix) `isPrefixOf` input
instance KnownSymbol substring => ValidateRule (Contains substring) String where
  validateRuleImpl _ input = symbolVal (Proxy @substring) `isInfixOf` input
instance KnownSymbol suffix => ValidateRule (EndsWith suffix) String where
  validateRuleImpl _ input = symbolVal (Proxy @suffix) `isSuffixOf` input

-- define what validations i want performed
data FileNameValidations = FileNameValidations
  { group :: BeginsWith "[BananaSubs]"
  , resolution :: Contains "[720p]"
  , extension :: EndsWith "mkv"
  } deriving (Generic)

-- define a function that only works when validations have been run
onlyOnGroupBananaSubs :: forall rules
  . HasField "group" (BeginsWith "[BananaSubs]") rules
 => ValidatedValue rules String
 -> String
onlyOnGroupBananaSubs (Const s) = "subbed by BananaSubs: " ++ s

checkRules' :: String -> IO ()
checkRules' s =
  case checkRules (Proxy @FileNameValidations) s of
    Success x -> putStrLn $ onlyOnGroupBananaSubs x
    Failure e -> putStrLn $ "failed to validate on keys: " ++ intercalate ", " e

main :: IO ()
main = do
  checkRules' "[AbogadoSubs] Tom Kha Gai [720p].avi"
  checkRules' "[BananaSubs] Phad Cha [720p].mkv"
  -- output:
  -- failed to validate on keys: group, extension
  -- subbed by BananaSubs: [BananaSubs] Phad Cha [720p].mkv
```
