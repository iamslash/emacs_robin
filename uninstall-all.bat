@ECHO OFF

REM default variables
SET EMACS_ROBIN_HOME=%HOME%\.emacs_robin
SET EMACS_FILE=%%HOME\.emacs

REM uninstall emacs_robin
rmdir /s /q %EMACS_ROBIN_HOME%
del /S %EMACS_FILE%

ECHO Success to uninstall emacs_robin
