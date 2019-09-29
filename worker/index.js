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
      '<meta name="viewport" content="width=device-width, user-scalable=no">' +
      '<link href="https://fonts.googleapis.com/css?family=Montserrat:400,500&display=swap" rel="stylesheet">' +
      '<title>421</title>' +
      '<link type="image/x-icon" rel="icon" href="data:image/x-icon;base64,PHN2ZyB3aWR0aD0iMTZweCIgaGVpZ2h0PSIxNnB4IiB2aWV3Qm94PSIwIDAgIDMyIDMyIj48cGF0aCBmaWxsPSJjdXJyZW50Q29sb3IiIGZpbGwtcnVsZT0ibm9uemVybyIgc3Ryb2tlPSJub25lIiBzdHJva2Utd2lkdGg9IjEiIGQ9Ik03LjM4IDUuNTU1bDE1LjU5Mi0xLjM2N0EzLjQxOSAzLjQxOSAwIDAxMjYuNjczIDcuM0wyOC4wNSAyMy4wNmEzLjQyMiAzLjQyMiAwIDAxLTMuMTA2IDMuNzFMOS4zNTIgMjguMTM3YTMuNDE5IDMuNDE5IDAgMDEtMy43MDItMy4xMTNMNC4yNzUgOS4yNjVhMy40MjIgMy40MjIgMCAwMTMuMTA2LTMuNzF6bS4yIDIuMjc0YTEuMTQgMS4xNCAwIDAwLTEuMDM2IDEuMjM3bDEuMzc1IDE1Ljc1OWExLjE0IDEuMTQgMCAwMDEuMjM0IDEuMDM4bDE1LjU5MS0xLjM2OGExLjE0IDEuMTQgMCAwMDEuMDM2LTEuMjM2bC0xLjM3Ni0xNS43NmExLjE0IDEuMTQgMCAwMC0xLjIzNC0xLjAzN0w3LjU4IDcuODI5em0zLjI1NCA1LjM5YTEuNjkgMS42OSAwIDAxLTEuODI1LTEuNTQ1IDEuNjkyIDEuNjkyIDAgMDExLjUzLTEuODQgMS42OSAxLjY5IDAgMDExLjgyNSAxLjU0NiAxLjY5MiAxLjY5MiAwIDAxLTEuNTMgMS44Mzl6bTEwLjA2NS0uODgzYTEuNjkgMS42OSAwIDAxLTEuODI2LTEuNTQ1IDEuNjkyIDEuNjkyIDAgMDExLjUzLTEuODQgMS42OSAxLjY5IDAgMDExLjgyNSAxLjU0NiAxLjY5MiAxLjY5MiAwIDAxLTEuNTMgMS44NHpNMTEuNzIgMjMuMzczYTEuNjkgMS42OSAwIDAxLTEuODI1LTEuNTQ1IDEuNjkyIDEuNjkyIDAgMDExLjUzLTEuODQgMS42OSAxLjY5IDAgMDExLjgyNSAxLjU0NSAxLjY5MiAxLjY5MiAwIDAxLTEuNTMgMS44NHptMTAuMDY1LS44ODNhMS42OSAxLjY5IDAgMDEtMS44MjUtMS41NDUgMS42OTIgMS42OTIgMCAwMTEuNTMtMS44NCAxLjY5IDEuNjkgMCAwMTEuODI1IDEuNTQ2IDEuNjkyIDEuNjkyIDAgMDEtMS41MyAxLjg0em0tNS40NzYtNC42MzVhMS42OSAxLjY5IDAgMDEtMS44MjUtMS41NDYgMS42OTIgMS42OTIgMCAwMTEuNTMtMS44MzkgMS42OSAxLjY5IDAgMDExLjgyNSAxLjU0NSAxLjY5MiAxLjY5MiAwIDAxLTEuNTMgMS44NHpNMjkuMTgzIDYuODIzbC0uMDE1LjAwMkEuOTE1LjkxNSAwIDAxMjguMTY3IDZjLS4yNjUtMi41NDQtMi41MjMtNC4zOS01LjA0NS00LjEyMWgtLjAwN2EuOTE2LjkxNiAwIDAxLTEuMDAyLS44MjQuOTIyLjkyMiAwIDAxLjgwOC0xLjAxOGguMDAybC4wMDctLjAwMWE2LjM4NyA2LjM4NyAwIDAxNC43MTggMS40MDggNi40OTggNi40OTggMCAwMTIuMzQ3IDQuMzYzLjkyMi45MjIgMCAwMS0uODEyIDEuMDE2ek04LjU0NyAzMmgtLjAwOGE2LjM5NSA2LjM5NSAwIDAxLTQuNTc4LTEuODE4IDYuNTEgNi41MSAwIDAxLTEuOTYtNC41NTMuOTIuOTIgMCAwMS44OTUtLjk0MmguMDE2Yy41MDMtLjAwOC45MTcuNC45MjYuOTEuMDQ0IDIuNTU5IDIuMTM0IDQuNTk1IDQuNjcgNC41NWguMDA2YS45MTguOTE4IDAgMDEuOTI3LjkxLjkyLjkyIDAgMDEtLjg5NC45NDN6Ij48L3BhdGg+PC9zdmc+Cg=="/>' + 
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

