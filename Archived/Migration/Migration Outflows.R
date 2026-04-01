# the following is necessary to scrape IRS SOI migration OUTFLOWS data.
#   INFLOWS data is in the unarchived script.


urls = c('https://www.irs.gov/statistics/soi-tax-stats-migration-data-2011-2012',
         'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2012-2013',
         'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2013-2014',
         'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2014-2015',
         'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2015-2016',
         'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2016-2017',
         'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2017-2018',
         'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2018-2019',
         'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2019-2020',
         'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2020-2021',
         'https://www.irs.gov/statistics/soi-tax-stats-migration-data-2021-2022')


outflowSelectors=c('body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > p:nth-child(8) > a:nth-child(5)',
                   'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(10) > li:nth-child(3) > a',
                   'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(9) > li:nth-child(3) > a',
                   'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(9) > li:nth-child(3) > a',
                   'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(9) > li:nth-child(3) > a',
                   'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(9) > li:nth-child(3) > a',
                   'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(9) > li:nth-child(3) > a',
                   'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(8) > li:nth-child(3) > a',
                   'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > ul:nth-child(9) > li:nth-child(3) > a',
                   'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > p:nth-child(8) > a:nth-child(3)',
                   'body > div.dialog-off-canvas-main-canvas > div.pup-main-container.container > div > div.pup-header-content-rt.col-sm-12.col-md-9 > div > article > div > div > p:nth-child(7) > a:nth-child(3)')

outflows = list()
for(i in 1:11){
  outflows[[i]] = loadFromPage(urls[i], outflowSelectors[i])
  Sys.sleep(.5)
}

