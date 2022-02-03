/* This function allows you to send a request to address `path`,
 with the given `method` and `body` */

 export async function sendRequest(methodOpt,headersOpt, keys, path) {
  
  var requestOptions = null;

  if (methodOpt === "POST") {
    requestOptions = {
      method: methodOpt,
      headers: headersOpt,
      body: keys
    }
    
  } else if (methodOpt === "GET") {
    requestOptions = {
      method: methodOpt,
      headers: headersOpt
    }
  } else if (methodOpt === "PUT") {
    requestOptions = {
      body: JSON.stringify(keys),
      headers: headersOpt,
      method: methodOpt
    }
  }
    console.log(requestOptions);
    return (await fetch(path, requestOptions))
  } 