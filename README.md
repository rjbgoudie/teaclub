# Teaclub r-package

The `teaclub` package automates all the dullest parts of administration of
the teaclub. Specifically, it handles:

1. Creating the blank sheet for people to write up what they've used
2. Creating a sheet to keep track of when people pay the teaclub
3. Creating conveniently-organised `csv` files in which to type-up what people
   have used.
4. Emailing people what they owe.

## Installation and prerequisites

The `teaclub` package requires Linux (or similar) to send emails (using
`mailx`) and compile the tex files (using `pdflatex`)

### R package dependencies
The R package requires the R packages `plyr` (for general data manipulation) and
`xtable` (for converting to LaTeX). Install them in R using
```R
> install.packages(c("plyr", "xtable"))
```
Then install the R package `teaclub`, which can be done on the command-line:
```bash
$ cd /path/to/teaclub/package/archive
$ R CMD INSTALL teaclub_0.1.tar.gz
```

### System dependencies
We can check if the emailing is going to work by running the following on the
Linux command line:
```bash
$ echo 'hello' | mailx -s 'test subject' testperson
```
This should email `testperson@...` (where `...` is defaults to the correct part
for internal emails) an email with subject "test subject" and contents "hello".
(This shorthand is used for internal people, so that just their Unix username
is needed.)

Similarly
```bash
$ echo 'hello' | mailx -s 'test subject' joe@example.com
```
should email joe@example.com the same message. (This us used for external
people.)

To check if the PDF compilation will work, check that `pdflatex` is installed.
On the Linux command line, run
```
$ pdflatex --version
```
It should print the version number if it is installed.

## Using teaclub

```R
owe_function <- function(x){
  owe_message <-
"Dear %s,

You currently owe the tea club £%.2f. This puts you in position %i from the top out of %i in the league.

If you owe more than £4.00, please could you pay the teaclub as soon as possible. If your bill is less than £4.00, please don't bother paying yet.

Thanks,
Teaclub"

  balance_round <- round(x$balance, 2)
  league_total <- nrow(x)
  x$message <- sprintf(owe_message,
                       x$display_name,
                       balance_round,
                       x$position,
                       league_total)
  x
}

credit_function <- function(x){
  credit_message <-
"Dear %s,

You are currently in credit by £%.2f. No action is therefore required.

Thanks,
Teaclub"

  credit_rounded <- -round(x$balance, 2)
  x$message <- sprintf(credit_message, x$display_name, credit_rounded)
  x
}


library(teaclub)

setwd("/path/to/teaclub/accounts/")
blank_people()
# 1. update people/today.csv with new people
# "unique_id" - MUST be unique across all time, since balances match on this
# "display_name" - What is shown on the tearoom sheet, and in emails
# "email" - An email address. "@mrc-bsu.cam.ac.uk" can be omitted
# "handwriting_factor" - fudge the number of cells in tearoom sheet

blank_sheet()
blank_paid()
blank_used()
# 2. print out sheet/today.pdf, replace in tearoom
# 3. update the paid file from paid/today.csv - the amounts are in PENCE
# 4. update the used file from used/today.csv - the amounts are in PENCE

display_used()

display_paid()

# (Note people not on this month's people.csv are excluded)
display_balances()

# (Note people not on this month's people.csv are NOT emailed)
# Check if message in owe_function and credit_function need updating
# Cross fingers... set dry_run = F to send emails
publish_balances(owe_function = owe_function,
                 credit_function = credit_function,
                 dry_run = T)
# print out owe/today.pdf
```
