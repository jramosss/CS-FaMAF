import React, { useState, useEffect } from 'react';
import '../../stylesheet/custom.css';

export const Director = (props) => {
    const[name, setName] = useState('')

    useEffect(() => {
        setName(props.name)
    }, [props]);

        return(
            <div class="container align-cntr my-1">
                <span class='i-playerlist'> <strong>Director:</strong> {name}</span>
            </div>
        )
}