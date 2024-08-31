module Domain.ValidationSpec (spec) where

import Domain.Authentication (
    EmailValidationError (InvalidEmailErr),
    PasswordValidationError (
        PasswordMustContainLowerCaseError,
        PasswordMustContainNumberError,
        PasswordMustContainUpperCaseError,
        PasswordTooShortError
    ),
    mkEmail,
    mkPassword,
    rawEmail,
    rawPassword,
 )
import Domain.Validation (lengthGreaterThan, regexMatch, validate)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Regex.PCRE.Heavy (re)

spec :: Spec
spec =
    describe "Domain.Validation" $ do
        it "checks input with `lengthGreaterThan`" $ do
            lengthGreaterThan 4 "err" "12345" `shouldBe` (Nothing :: Maybe String)
            lengthGreaterThan 4 "err" "1234" `shouldBe` (Just "err" :: Maybe String)
            lengthGreaterThan 4 PasswordTooShortError "12345" `shouldBe` Nothing
            lengthGreaterThan 4 PasswordTooShortError "1234" `shouldBe` Just PasswordTooShortError
        it "checks an email address is valid" $ do
            regexMatch [re|@|] InvalidEmailErr "hello" `shouldBe` Just InvalidEmailErr
            regexMatch [re|@|] InvalidEmailErr "hello@example.com" `shouldBe` Nothing
        it "can combine `Validation`s with `validate`" $ do
            -- Fonctions are partially applied here...
            let mustContainA = regexMatch [re|A|] "Must contain 'A'"
            let mustContainB = regexMatch [re|B|] "Must contain 'B'"
            validate id [mustContainA, mustContainB] "abc" `shouldBe` Left (["Must contain 'A'", "Must contain 'B'"] :: [String])
            validate id [mustContainA, mustContainB] "Abc" `shouldBe` Left (["Must contain 'B'"] :: [String])
            validate id [mustContainA, mustContainB] "ABc" `shouldBe` Right "ABc"
        describe "`mkEmail` tries to assess if the given email address looks mostly correct" $ do
            it "returns an error when the input is obviously incorrect (nothing smart though)" $ do
                mkEmail "user" `shouldBe` Left [InvalidEmailErr]
                rawEmail <$> mkEmail "user@example.com" `shouldBe` Right "user@example.com"
                mkEmail "user@" `shouldBe` Left [InvalidEmailErr]
                mkEmail "user@a." `shouldBe` Left [InvalidEmailErr]
            it "accepts possibly ok email addresses" $ do
                -- Better to let in a possibly wrong email address, rather than block a possibly correct email address
                rawEmail <$> mkEmail "user@a.c" `shouldBe` Right "user@a.c"
                rawEmail <$> mkEmail "user@example.com" `shouldBe` Right "user@example.com"
                rawEmail <$> mkEmail "é@example.com" `shouldBe` Right "é@example.com"
            describe "`mkPassword`" $ do
                it "will reject an invalid password" $
                    mkPassword ""
                        `shouldBe` Left
                            [ PasswordTooShortError
                            , PasswordMustContainNumberError
                            , PasswordMustContainLowerCaseError
                            , PasswordMustContainUpperCaseError
                            ]
                it "will accept a valid password" $ do
                    let allErrors = [PasswordTooShortError, PasswordMustContainNumberError, PasswordMustContainLowerCaseError, PasswordMustContainUpperCaseError]
                    (rawPassword <$> mkPassword "") `shouldBe` Left allErrors
                    (rawPassword <$> mkPassword "123456789Ab") `shouldBe` Right "123456789Ab"