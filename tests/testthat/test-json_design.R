context("test-json_design")

# defaults ----
test_that("defaults", {
  des <- check_design(mu = 1.123456789, plot = FALSE)
  json <- json_design(des)
  txt <- '{"within":[],"between":[],"dv":{"y":"value"},"id":{"id":"id"},"n":{"y":100},"mu":{"y":{"y":1.12345679}},"sd":{"y":{"y":1}},"r":[],"sep":"_"}'
  class(txt) <- "json"
  
  expect_equal(json, txt)
  
  des <- check_design(2,2, plot = FALSE)
  json <- json_design(des)
  txt <- '{"within":{"A":{"A1":"A1","A2":"A2"}},"between":{"B":{"B1":"B1","B2":"B2"}},"dv":{"y":"value"},"id":{"id":"id"},"n":{"B1":100,"B2":100},"mu":{"B1":{"A1":0,"A2":0},"B2":{"A1":0,"A2":0}},"sd":{"B1":{"A1":1,"A2":1},"B2":{"A1":1,"A2":1}},"r":{"B1":[[1,0],[0,1]],"B2":[[1,0],[0,1]]},"sep":"_"}'
  class(txt) <- "json"
  
  expect_equal(json, txt)
})

# filename ----
test_that("filename", {
  des <- check_design(plot = FALSE)
  json <- json_design(des, "test.json")
  des2 <- jsonlite::read_json("test.json")
  json2 <- jsonlite::toJSON(des2, auto_unbox = TRUE)
  des$params <- NULL
  expect_equivalent(des, des2)
  expect_equal(json, json2)
  
  file.remove("test.json")
  
  # no .json suffix
  json <- json_design(des, "test")
  des2 <- jsonlite::read_json("test.json")
  json2 <- jsonlite::toJSON(des2, auto_unbox = TRUE)
  des$params <- NULL
  expect_equivalent(des, des2)
  expect_equal(json, json2)
  
  file.remove("test.json")
})

# digits ----
test_that("digits", {
  des <- check_design(mu = 1.123456789, plot = FALSE)
  json <- json_design(des, digits = 3)
  txt <- '{"within":[],"between":[],"dv":{"y":"value"},"id":{"id":"id"},"n":{"y":100},"mu":{"y":{"y":1.123}},"sd":{"y":{"y":1}},"r":[],"sep":"_"}'
  class(txt) <- "json"
  
  expect_equal(json, txt)
  
  json <- json_design(des, digits = 4)
  txt <- '{"within":[],"between":[],"dv":{"y":"value"},"id":{"id":"id"},"n":{"y":100},"mu":{"y":{"y":1.1235}},"sd":{"y":{"y":1}},"r":[],"sep":"_"}'
  class(txt) <- "json"
  
  expect_equal(json, txt)
})

# pretty ----
test_that("pretty", {
  des <- check_design(plot = FALSE)
  json <- json_design(des, pretty = TRUE)
  txt <- '{
  "within": [],
  "between": [],
  "dv": {
    "y": "value"
  },
  "id": {
    "id": "id"
  },
  "n": {
    "y": 100
  },
  "mu": {
    "y": {
      "y": 0
    }
  },
  "sd": {
    "y": {
      "y": 1
    }
  },
  "r": [],
  "sep": "_"
}'
  
  class(txt) <- "json"
  expect_equal(json, txt)
})
