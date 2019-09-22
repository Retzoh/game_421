addEventListener('fetch', event => {
  event.respondWith(handleRequest(event.request))
})

/**
 * Fetch and log a request
 * @param {Request} request
 */
async function handleRequest(request) {
  return new Response(
    html, 
    { headers: { 'Content-Type': 'text/html' }, status: 200 }
  )
}

const html = '<!DOCTYPE html>' +
'<html>' +
    '<head>' +
      '<meta charset="UTF-8">' +
      '    <meta name="viewport" content="width=device-width, user-scalable=no">' +
      '<title>Main</title>' +
    '</head>' +
'' +
    '<body>' +
      '<div id="elm"></div>' +
      '<script>' + '\
    ELM_CODE
' +
          'var app = Elm.Main.init({' +
              'node: document.getElementById("elm")' +
          '});' +
        '</script>' +
    '</body>' +
'</html>'

