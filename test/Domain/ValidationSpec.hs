module Domain.ValidationSpec (spec) where

import Data.Text (unpack)
import Domain.Authentication (
    Email (Email),
    EmailValidationError (InvalidEmailErr),
    PasswordValidationError (PasswordMustContainLowerCaseError, PasswordMustContainNumberError, PasswordMustContainUpperCaseError, PasswordTooShortError),
    mkEmail,
    mkPassword,
 )
import Domain.Validation (lengthBetween, regexMatch, validate)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Text.Regex.PCRE.Heavy (re)

spec :: Spec
spec =
    describe "Domain.Validation" $ do
        it "checks input with `lengthBetween`" $ do
            lengthBetween 1 5 "err" "12345" `shouldBe` (Nothing :: Maybe String)
            lengthBetween 1 3 "err" "12345" `shouldBe` (Just "err" :: Maybe String)
            lengthBetween 1 5 PasswordTooShortError "12345" `shouldBe` Nothing
            lengthBetween 1 3 PasswordTooShortError "12345" `shouldBe` Just PasswordTooShortError
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
                mkEmail "user@example.com" `shouldBe` Right (Email "user@example.com")
                mkEmail "user@" `shouldBe` Left [InvalidEmailErr]
                mkEmail "user@a." `shouldBe` Left [InvalidEmailErr]
            it "accepts possibly ok email addresses" $ do
                -- Better to let in a possibly wrong email address, rather than block a possibly correct email address
                mkEmail "user@a.c" `shouldBe` Right (Email "user@a.c")
                mkEmail "user@example.com" `shouldBe` Right (Email "user@example.com")
                mkEmail "é@example.com" `shouldBe` Right (Email "é@example.com")
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
                    let validInput = "123456789aA"
                    case mkPassword validInput of
                        Left err -> expectationFailure ("The password '" ++ unpack validInput ++ "' should have passed: " ++ show err)
                        Right _pw -> return ()
