library(aws.s3)

Sys.setenv("AWS_ACCESS_KEY_ID" = "XXXXXX",
           "AWS_SECRET_ACCESS_KEY" = "XXXXX",
           "AWS_DEFAULT_REGION" = "eu-central-1")

fileObject <- get_object("s3://db-name/raw/contract/filename")

