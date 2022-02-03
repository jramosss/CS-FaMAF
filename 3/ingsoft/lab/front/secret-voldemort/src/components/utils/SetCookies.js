import Cookies from 'js-cookie';

/* This function takes care of setting cookies */

export function SetCookies(name, username, token, email, icon) {

  if(name !== '' && username !== '' && token !== ''
    && email !== '') {

      Cookies.set(name, {
        username: username,
        token: token,
        email: email,
        icon: icon,
        room_name: ''
      })

  } else {
    console.log("Cookie not set correctly, empty fields")
  }

}