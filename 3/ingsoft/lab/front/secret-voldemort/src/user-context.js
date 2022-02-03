import { createContext } from 'react';

export const userContext = createContext({
    token: '',
    username: '',
    email: '',

    setToken: () => {},
    setUsername: () => {},
    setEmail: () => {},
});





 
