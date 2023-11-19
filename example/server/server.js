const http = require('http');

const hostname = '0.0.0.0';
const port = 3000;

const server = http.createServer((req, res) => {
  res.statusCode = 200;
  res.setHeader('Content-Type', 'text/plain');

  let body = "";
  let keys = Object.keys(process.env)
  keys.sort();
  for (let key of keys) {
    body += key + "=" + process.env[key] + "\n";
  }
  res.end(body);
});

server.listen(port, hostname, () => {
  console.log(`Server running at http://${hostname}:${port}/`);
  console.log(process.env)
});
