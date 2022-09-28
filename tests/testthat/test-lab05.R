library(httr)
library(jsonlite)
library(tidyverse)

rmm<-get_data(link="http://api.kolada.se/v2/data/",kpi="N00945",period="2009")

data = return_df(rmm)
jsono_text = return_json(rmm)

test_that("link should be an web api link", {
  expect_error(get_data(111))
  expect_error(get_data("abcde"))
})

test_that("Data is correct", {
  expect_true(nrow(data) == 3549)
  expect_true(data$value[1] == 35.55521144)
  expect_true(jsono_text[[2]] == 200)
})

test_that("Datastruct is correct", {
  expect_true(is.list(rmm))
  expect_true(is.data.frame(data))
  expect_true(is.list(jsono_text))
  expect_true(is.numeric(data$value[[1]]))
})

test_that("2 in kpi,municipality,period should be given", {
  expect_error(get_data(link="http://api.kolada.se/v2/data/",kpi="N00945"))
  expect_error(get_data(link="http://api.kolada.se/v2/data/",period="2009"))
})


test_that("the data contains the right attribute", {
  expect_true(ncol(data)==7)
  expect_true(all(colnames(data)==c("kpi","municipality","period","count","gender","status","value")))
})
