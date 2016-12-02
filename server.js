var express = require('express');
var bodyParser = require('body-parser');

var app = express();
app.use(bodyParser.json());

app.use(express.static('public'));

app.get('/canvas/:id', function(req, res, next) {
  res.send('get');
});

app.post('/canvas', function(req, res, next) {
  res.send('post');
});

app.listen(process.env.PORT || 3000, function () {
    console.log('Example app listening on port 3000!');
});
