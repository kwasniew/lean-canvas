var express = require('express');
var bodyParser = require('body-parser');
var uuid = require('node-uuid');

var app = express();
app.use(bodyParser.json({limit: '10kb'}));

app.use(express.static('public'));

var db = {};

app.get('/canvas/:id', function(req, res, next) {
  if(db[req.params.id]) {
    res.json(db[req.params.id]);
  }
  res.status(404);
});

app.post('/canvas', function(req, res, next) {
  req.body.id = uuid.v4();
  db[req.body.id] = req.body;

  res.send(req.body.id);
});

app.listen(process.env.PORT || 3000, function () {
    console.log('Example app listening on port 3000!');
});
