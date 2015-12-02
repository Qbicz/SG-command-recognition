SET ROPTS=--no-save --no-environ --no-init-file --no-restore --no-Rconsole
R\bin\Rscript.exe %ROPTS% run\rs.R> log\shinyError.log 2>&1