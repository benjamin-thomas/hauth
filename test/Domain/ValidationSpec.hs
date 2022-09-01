module Domain.ValidationSpec (spec) where

import Data.Text (unpack)
import Domain.Authentication (
    Email (Email),
    EmailValidationError (InvalidEmailErr),
    PasswordValidationError (PasswordMustContainLowerCaseError, PasswordMustContainNumberError, PasswordMustContainUpperCaseError, PasswordTooShortError),
    mkEmail,
    mkPassword,
    rawPassword,
 )
import Domain.Validation (lengthLessThan, regexMatch, validate)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Text.Regex.PCRE.Heavy (re)

spec :: Spec
spec =
    describe "Domain.Validation" $ do
        it "checks input with `lengthLessThan`" $ do
            lengthLessThan 5 "err" "12345" `shouldBe` (Nothing :: Maybe String)
            lengthLessThan 5 "err" "1234" `shouldBe` (Just "err" :: Maybe String)
            lengthLessThan 5 PasswordTooShortError "12345" `shouldBe` Nothing
            lengthLessThan 5 PasswordTooShortError "1234" `shouldBe` Just PasswordTooShortError
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
                    let allErrors = [PasswordTooShortError, PasswordMustContainNumberError, PasswordMustContainLowerCaseError, PasswordMustContainUpperCaseError]
                    (rawPassword <$> mkPassword "") `shouldBe` Left allErrors
                    (rawPassword <$> mkPassword "123456789Ab") `shouldBe` Right "123456789Ab"