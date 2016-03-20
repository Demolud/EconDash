Randomize
CreateObject("Wscript.Shell").Run "C:\RDash\R-Portable\App\R-Portable\bin\R.exe CMD BATCH --vanilla --slave runShinyApp.R" & " " & RND & " ", 0, False
