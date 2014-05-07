/* 
http://codeflow.org/entries/2013/feb/02/why-you-should-use-webgl/
http://learningwebgl.com/lessons
http://glsl.heroku.com/
https://github.com/KhronosGroup/WebGL
http://www.reddit.com/r/webgl

Alternatives to webGL
http://codeflow.org/entries/2013/feb/02/why-you-should-use-webgl/#unity-3d

http://www.yorkshireeveningpost.co.uk/news/latest-news/top-stories/global-warming-is-rubbish-says-top-professor-1-6536732

http://www.natureworldnews.com/articles/6802/20140429/former-nasa-scientist-says-climate-change-is-nonsense.htm

# Compare concurrency techniques:
  messaging
  publish and subscribe
  no sql
  distributed transactions

Get a Canvas:
*/

var canvas = document.createElement('canvas');
document.body.appendChild(canvas);

/* Get a context:
*/

var gl = canvas.getContext('experimental-webgl');

