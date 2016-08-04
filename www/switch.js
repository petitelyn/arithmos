/*this function switches the UI based on what stage it is in
*
* normally a dynamic ui would be accomplished using multiple html pages
* but the way shiny builds its app is all in a single html page made that difficult
* as I am an inexperienced front end developer, there is probably a better way to accomplish this 
* 
* stages:
*
* -load: the first page with both the upload and search across panels. load also is called with resetting the app
* -process: the data processing page, before processing data
* -view: the data processing page, after processing data
* -analysis: page with all data analysis, both with its own lefthand panel and all of the graphs etc 
* 
*/
Shiny.addCustomMessageHandler ('switch', function (stage) {
    if (stage == 'load') {
      $('#refresh').show()
      $('#uploadAndLoad').show()
      $('#acrossSearchPanel').show()
      $('#processing').hide()
      $('#restart').hide()
      $('#mergedDataPanel').hide()
      $('#viewAndBegin').hide()
      $('#analysisSidebar').hide()
      $('#analysisPanel').hide()
    } else if (stage == 'process'){
      $('#refresh').hide()
      $('#uploadAndLoad').hide()
      $('#restart').show()
      $('#processing').show()
      $('#acrossSearchPanel').hide()
    } else if (stage == 'view'){
      $('#viewAndBegin').show()
      $('#mergedDataPanel').show()
    } else if (stage == 'analysis'){
      $('#acrossSearchPanel').hide()
      $('#mergedDataPanel').hide()
      $('#processing').hide()
      $('#viewAndBegin').hide()
      $('#analysisSidebar').show()
      $('#analysisPanel').show()
    }
    
 });