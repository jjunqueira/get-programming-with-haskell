module Chapter12 where

type FirstName = String
type MiddleName = String
type LastName = String
data Name = Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName

type PatientName = (String, String)
type Age = Int
type Height = Int

data Sex = Male | Female

data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType

data Patient = Patient { name :: Name
                       , sex :: Sex
                       , age :: Int
                       , height :: Int
                       , weight :: Int
                       , bloodType :: BloodType }

firstName :: PatientName -> String
firstName patient = fst patient

lastName :: PatientName -> String
lastName patient = snd patient

showName :: Name -> String
showName (Name f l) = l ++ ", " ++ f
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

sexInitial :: Sex -> String
sexInitial Male = "M"
sexInitial Female = "F"

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"
showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateToBt :: BloodType -> BloodType -> Bool
canDonateToBt (BloodType O _) _ = True
canDonateToBt _ (BloodType AB _) = True
canDonateToBt (BloodType A _) (BloodType A _) = True
canDonateToBt (BloodType B _) (BloodType B _) = True
canDonateToBt _ _ = False --otherwise

canDonateTo :: Patient -> Patient -> Bool
canDonateTo p1 p2 = canDonateToBt (bloodType p1) (bloodType p2)

patientInfo :: PatientName -> Age -> Height -> String
patientInfo (fname, lname) age height = name ++ " " ++ ageHeight
    where name = lname ++ ", " ++ fname
          ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

patientSummary :: Patient -> String
patientSummary p1 =
  "**********" ++ "\n" ++
  "Patient Name: " ++ showName (name p1) ++ "\n" ++
  "Sex: " ++ sexInitial (sex p1) ++ "\n" ++
  "Age: " ++ show (age p1) ++ "\n" ++
  "Weight: " ++ show (weight p1) ++ " lbs." ++ "\n" ++
  "Blood Type: " ++ showBloodType (bloodType p1) ++ "\n" ++
  "**********" ++ "\n"

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

name1 = Name "Jerome" "Salinger"
name2 = NameWithMiddle "Jerome" "David" "Salinger"

jackieSmith :: Patient
jackieSmith = Patient { name = Name "Jackie" "Smith"
                      , age = 43
                      , sex = Female
                      , height = 62
                      , weight = 115
                      , bloodType = BloodType O Neg }

janeDoe :: Patient
janeDoe = Patient { name = Name "Jane" "Doe"
                      , age = 43
                      , sex = Female
                      , height = 62
                      , weight = 115
                      , bloodType = BloodType O Neg }
