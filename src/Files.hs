module Files (files, contact, aboutthis) where

import qualified Data.Map as M

files = M.fromList [("Contact.md", contact), ("AboutThis.md", aboutthis)]

contact = "\nzach_asmith@yahoo.com  @casserolezach  (248) 917-1520\n"

aboutthis = "\nREADME: github.com/aryzach/sshServer\n"

