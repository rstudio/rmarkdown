context('utility functions')

test_that('default_geometry() decides whether to pass a default geometry variable to Pandoc', {
  expect_false(default_geometry('geometry'))
  expect_false(default_geometry('documentclass'))
  expect_false(default_geometry(c('geometry', 'bibliography')))
  expect_true(default_geometry('output', 'classoption'))

  expect_false(default_geometry('fontsize', '--variable=documentclass:book'))
  expect_false(default_geometry('fontsize', '--metadata=documentclass:book'))
  expect_false(default_geometry('fontsize', c('--variable', 'documentclass:book')))
  expect_true(default_geometry('fontsize', c('--variable', 'graphics:true')))
})
