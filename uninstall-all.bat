@ECHO OFF

REM default variables
SET EMACS_ROBIN_HOME=c:\iamslash\.emacs_robin
SET EMACS_FILE=c:\iamslash\.emacs

REM uninstall emacs_robin
rmdir /s /q %EMACS_ROBIN_HOME%
del /S %EMACS_FILE%

ECHO Success to uninstall emacs_robin
