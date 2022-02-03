import React, { useState } from 'react';
import { Redirect } from 'react-router-dom';

export const RoomTag = props =>{

  const [room, chooseRoom] = useState('');
  
  const rootpath = '/joinRoom/'
  
  return (
    (props.room.active_users !== props.room.max_players) 
    ?
      <div class='container my-2 room-row'>
        {
          (room === '' 
          ?
            <div id={props.room.name} class='columns py-3 is-vcentered align-cntr'>
              <div id='name' class='column is-6'>
                {props.room.name}
              </div>
              <div id='#players' class='column is-2'>
                {props.room.active_users}/{props.room.max_players}
              </div>
              <div class='column is-4'>
                <button id={`${props.room.name}join`} class='room-button' onClick={() => {chooseRoom(props.room.name)}}>Unirse</button>
              </div>
            </div>
          :
          <Redirect to={rootpath+room}/>)
        }
      </div>
    :
      <div></div>
      
  );
}