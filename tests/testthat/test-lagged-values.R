test_that("many lagged values", {
  data = rbind(
    as.data.frame(list(
      y = 4,
      x1 = 3,
      x2 = 5,
      p = '2019-01-01'
    )),
    as.data.frame(list(
      y = 2,
      x1 = 3,
      x2 = 4,
      p = '2018-12-01'
    )),
    as.data.frame(list(
      y = 4,
      x1 = 2,
      x2 = 2,
      p = '2018-11-01'
    )),
    as.data.frame(list(
      y = 6,
      x1 = 1,
      x2 = 1,
      p = '2018-10-01'
    )),
    as.data.frame(list(
      y = 7,
      x1 = 3,
      x2 = 2,
      p = '2018-09-01'
    )),
    as.data.frame(list(
      y = 2,
      x1 = 3,
      x2 = 4,
      p = '2018-08-01'
    )),
    as.data.frame(list(
      y = NA,
      x1 = 2,
      x2 = 2,
      p = '2018-07-01'
    )),
    as.data.frame(list(
      y = NA,
      x1 = 1,
      x2 = 1,
      p = '2018-06-01'
    )),
    as.data.frame(list(
      y = NA,
      x1 = NA,
      x2 = 2,
      p = '2018-05-01'
    )),
    as.data.frame(list(
      y = NA,
      x1 = NA,
      x2 = 7,
      p = '2018-04-01'
    ))
  )

  result = manyLaggedValues(data,
                            c('x1','x2'),
                            'p',
                            2)
  expect(is.na(result[10,'x1L2']),'Not NA')
  expect_equal(result[8,'x2L2'], 7)
  expect_equal(result[6,'x1L2'], 1)

})
