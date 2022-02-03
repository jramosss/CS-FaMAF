import React, { useState, useEffect, memo } from 'react';


/* This component take a list of user with their votes as
 * [{user: "username", vote: "lumox/nox"},...] */
 export function VotesList(props) {
    
  const [usersVotes, setUsersVotes] = useState([])

  useEffect(() =>{
    setUsersVotes(props.usersVotes)
  },[props]);

  return(
    <div class='container has-text-centered'>
      <ul>
        {usersVotes.map(user =>
          <li class='i-playerlist'>{user.user} votes: {user.vote}</li>
        )}
      </ul>
    </div>
  );
}