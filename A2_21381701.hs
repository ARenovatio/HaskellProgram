-- data types
import System.IO
import Data.List
type Name = String
type AccountNumber = String
type ID = Int
type Value = Float
type Currency = String
type TransactionAmount = Float
type ReferenceNumber = Int
type Comment = String

type Account = (ID, Name, AccountNumber, Currency, Value)
type AccountList = [Account]

type Transaction = (AccountNumber, TransactionAmount, ReferenceNumber, Comment)
type TransactionList =[Transaction] 

-- Initially populated list
myList :: AccountList
myList = [(1, "Alessandro", "21381701", "GBP", 100.00)]
bank :: TransactionList
bank = [("21381701", 50.00, 1, "Rent")]


-- "ui": part of main input processing; prompts user for a command,
-- then hands command and contact list to handle function.
-- Contact list must be retained as data (no global variables)
-- input: ContactList (data to operate on)
main = ui bank
ui :: TransactionList -> IO ()
ui ts = do
 putStrLn "Welcome to bank, what operation would you like to do? [deposit, list , withdraw, quit]"
 cmd <- getLine
 handle cmd ts
-- "handle": part of main input processing; uses pattern matching to handle input commands
-- input: String (command), TransactionList (data to operate on)
handle :: String -> TransactionList -> IO ()
handle "quit" ts = do
 putStrLn "Goodbye!"
handle "list" ts = do
 displayTransaction ts
 ui ts
handle "deposit" ts = do
 putStrLn "Please enter account number: "
 newAccountNumber <- getLine
 putStrLn "Please enter a value: "
 interimValue <- getLine
 let newValue = read interimValue :: Float
 putStrLn "Please enter a comment: "
 comment <- getLine
 putStrLn "Please enter currency [USD, EUR, GBP]"
 curr <- getLine
 let finalValue = depositexchange newValue curr 
 ui ((newAccountNumber, finalValue, (length ts) + 1, "Deposit: " ++ comment): ts)
handle "withdraw" ts = do
 putStrLn "Please enter account number: "
 newAccountNumber2 <- getLine
 putStrLn "Please enter a value: "
 interimValue2 <- getLine
 let newValue2 = read interimValue2 :: Float
 putStrLn "Please enter a comment: "
 comment2 <- getLine
 putStrLn "Please enter currency [USD, EUR, GBP]"
 curr2 <- getLine
 let finalValue2 = exchange newValue2 curr2 
 ui ((newAccountNumber2, finalValue2, (length ts) + 1, "Withdrawl: " ++ comment2): ts)
handle x ts = do
 putStrLn (x ++ " is not recognised.")
 ui ts


-- "displayTransaction" prints transactions list on the screen
-- input: TransactionList
displayTransaction :: TransactionList -> IO ()
displayTransaction [] = do
 putStrLn "List of Transaction:"
 putStrLn "All final values in GBP"
 putStrLn "=================="
displayTransaction ((accnumber, tamount, ref, comment):ts) = do
 displayTransaction ts
 putStrLn ("   Reference: " ++ (show ref))
 putStrLn ("   Account no: " ++ (show accnumber))
 putStrLn ("   Value: " ++ (show tamount))
 putStrLn ("   comment: " ++ comment)
 putStrLn "=================="


-- This converts the deposit value into the fixed rate currency exchange
depositexchange :: Float -> String -> Float
depositexchange x "USD" = x / 1.2
depositexchange x "EUR" = x / 1.25
depositexchange x "GBP" = x * 1

-- This converts the value into the fixed rate currency exchange
exchange :: Float -> String -> Float
exchange x "USD" = x * 1.2
exchange x "EUR" = x * 1.25
exchange x "GBP" = x * 1
