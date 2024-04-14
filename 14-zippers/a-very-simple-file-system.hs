import Data.List (break)

type Name = String
type Data = String
-- A new data type to represent files or folders.
-- The type File can have two string, the first one is the name of the file and
-- the second one is the data it contains.
-- The type Folder can have a string to represent the name of the folder and a list
-- of FSItem, that can either be a file or another folder.
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem
myDisk = 
  Folder "root" [
    File "goat_yelling_like_man.wmv" "baaaaaa",
    File "pope_time.avi" "god bless",
    Folder "pics" [
      File "ape_throwing_up.jpg" "bleargh",
      File "watermelon_smash.gih" "smash!!",
      File "skull_man(scary).bmp" "Yikes!"
    ],
    File "dijon_poupon.doc" "best mustard",
    Folder "programs" [
      File "fartwizard.exe" "10gotofart",
      File "owl_bandit.dmg" "mov eax, h00t",
      File "not_a_virus.exe" "really not a virus",
      Folder "source code" [
        File "best_hs_prog.hs" "main = print (fix error)",
        File "random.hs" "main = print 4"
      ]
    ]
  ] 

-- This data type is to represent the breadcrumb for the file system.
-- The 'Name' is the name of the parent folder.
-- The first list is for the items that come before the current file/folder.
-- The second list is for the items that come after the current file/folder.
-- So we have something like parent -> before __ after, where the empty space is
-- the current file/folder we are.
data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

-- This is the Zipper structure for the File system. The first element of the tuple
-- it the file/folder we're in, and the second element is the list that contains 
-- all breadcrumbs necessary to reconstruct the file system.
type FSZipper = (FSItem, [FSCrumb])

-- This go to up in the file system
fsUp :: FSZipper -> FSZipper
-- Item here is the focus element, name is the parent, ls is the item that came before,
-- and rs:bs are the elements the come after.
-- This is going back to the parent folder.
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

-- Takes a 'Name' and a FSItem and return True if there's a Folder or a File with the 
-- same name as 'Name'
nameIs :: Name -> FSItem -> Bool 
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

-- Takes a name and a 'FSZipper'. What it returns is a new 'FSZipper' that has its 
-- focus in the 'FSItem' with the same name as 'Name'
fsTo :: Name -> FSZipper -> FSZipper
-- Destructure the 'FSZipper' with pattern matching.
fsTo name (Folder folderName items, bs) =
-- 'break' returns a pair of lists, the first is for where the predicate is false and
-- the second one is first element that returned true, alongside with all the other
-- elements after that.
-- Pattern match the return of the function 'break' into the first element, and the
-- head and tail of the second list.
  let (ls, item:rs) = break (nameIs name) items
-- returrn a tuple where the first element is the first match for the name that 
-- the function received.
-- The second element is a 'FSCrumb' that contains the 'folderName', that were 
-- previously the focus and now is the parent, 'ls' that are the items before the
-- now current focus, and 'rs:bs' that are the elements that come after the focus.
  in (item, FSCrumb folderName ls rs:bs)

-- let newFocus = (myDisk,[]) -: fsTo "pics" -: fsTo "skull_man(scary).bmp"  
-- fst newFocus  
-- File "skull_man(scary).bmp" "Yikes!"

-- let newFocus2 = newFocus -: fsUp -: fsTo "watermelon_smash.gif"  
-- fst newFocus2  
-- File "watermelon_smash.gif" "smash!!"

-- A function that renames the current focus item
fsRename :: Name -> FSZipper -> FSZipper
-- Uses pattern match to first check if its a Folder or a File, and then rename it.
fsRename newName (Folder name items, bs) = (Folder newName items, bs)
fsRename newName (File name dat, bs) = (File newName dat, bs)

-- let newFocus = (myDisk,[]) -: fsTo "pics" -: fsRename "cspi" -: fsUp  


-- A function that adds a new file to the focus folder
fsNewFile :: FSItem -> FSZipper -> FSZipper
-- Deconstruct the 'FSZipper' to have access to the list of items of the folder
fsNewFile item (Folder folderName items, bs) =
-- Add the 'item' as the head of the list of folders and items.
  (Folder folderName (item:items), bs)

-- let newFocus = (myDisk,[]) -: fsTo "pics" -: fsNewFile (File "heh.jpg" "lol") -: fsUp  
