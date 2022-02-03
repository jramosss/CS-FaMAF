import React, { useContext, useEffect, useState }from 'react';
import { Redirect } from 'react-router-dom';
import { userContext } from '../../user-context';
import Cookie from 'js-cookie';
import { sendRequest } from '../../services/request';
import { RoomTag } from './RoomTag';

export const ListRoom = props => {

  const [roomList, updateRoomlist] = useState([]);
  const [redirect, Back] = useState(false);

  const context = useContext(userContext);

  useEffect(() => {
    if (context.token === '') {
      const cookie = Cookie.getJSON('user');
      if (cookie !== undefined){
        context.setToken(cookie.token);
        context.setUsername(cookie.token);
        context.setEmail(cookie.email);
        if (roomList.length === 0) {
          document.getElementById("refresh").click()
        }
      } 
    }
  }, [context]);

  const getRooms = () => {
    const method = "GET";
    const header = {
      Accept: "application/json",
      Authorization: "Bearer " + context.token
    }

    sendRequest(method, header, '',"http://localhost:8000/rooms")
      .then(async response => {
        const data = await response.json();
        
        if (response.ok) {
          console.log("List of rooms received");
          updateRoomlist(data.room_list);
        } else {
          console.log("Failed on obtain the list of rooms");
        }
      }).catch(error => {
        console.log("There was an error on obtain the list of rooms");
      });
  }

  return(
    <userContext.Consumer>
      {token => (
        (Cookie.get('user') !== undefined && !redirect) ? 
          <div class='container my-3 py-3'>
            <div class='column is-2 is-offset-2'>
              <button id='back' class='room-button' type='button' onClick={()=>Back(true)}> Back </button>
            </div>
            <div class='column is-5 is-offset-4'>
              <div class='card'>
                <div class='card-header'>
                  <p id='title' class='card-header-title room-title is-centered'>
                    Select a room
                  </p>
                </div>
                <div class='card-content'>
                  <div class='column'>
                    {
                      <div class='table'>
                        <div class='table-body'>
                          {
                            roomList !== [] ? roomList.map(room =>
                              <div class='tr'><RoomTag room={room}/></div>
                            )
                            :
                            <div></div>
                          }
                        </div>
                      </div>
                    }
                  </div>
                </div>
                <div class='card-footer'>                 
                  <button id='refresh' class='card-footer-item room-button is-medium' type='button' onClick={getRooms}> Refresh</button>
                </div>
              </div>
            </div>
          </div>
        :
          <Redirect to='/'/>
      )}
    </userContext.Consumer>
    );
}