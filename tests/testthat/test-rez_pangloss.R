test_that("rez_pangloss works", {
  rez_pangloss("https://cocoon.huma-num.fr/data/mazaudon/masters/crdo-TAJ_ORIGTAM.xml", "tamang1.txt")
  expect_equal(1, 1) #change later
})
