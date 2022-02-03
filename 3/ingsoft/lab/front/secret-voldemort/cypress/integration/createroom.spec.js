
/* 
  To be able to run these tests it is necessary to install certain dependencies: 
      npm install cypress --save-dev 
  To run these tests use the command and select the test: 
      npm run test:integration
*/

context('Create a new room', () => {

  // https://on.cypress.io/interacting-with-elements
  describe('User is not logged in', () => {
    
    it('Try to access the createroom page\n [*] should render the login page', () => {
      cy.visit('http://localhost:3000/createRoom')
      cy.get('h2')
      .contains('To play you need to be logged in')
    })

  })

  describe('The user is logged in', () => {
    beforeEach(() => {
      // We login with a user to be able to test the component correctly
      cy.visit('/')

      cy.get('input[type=email]')
        .type('player1@example.com')
      
      cy.get('input[type=password]')
        .type('Heladera65')
  
      cy.get('input[type=submit]').click()
    })

    /* It checks the correct rendering of the page */
    it('Click on create room\n [*] should render the create room page', () => {
      cy.get('button[id=create]')
        .click()
      cy.get('h2')
        .should('be.visible')
        .and("contain", 'Creation of room')
      cy.get('input[value="Create room"]')
        .should('be.visible')
      cy.contains('Cancel')
        .should('be.visible')
    })

    /* Cookies are checked for functionality when refreshing the page */
    it('Reload the page\n [*] should render the home_page again after reloading', () => {
      cy.wait(500)
      
      cy.reload()
      
      cy.get('button[id=logout]')
      .should('be.visible')
    })

    /* It is checked that you cannot create a room with a name less than 6 characters */
    it('Try to put a name < 6 characters\n [*] the room should not be created', () => {
      cy.get('button[id=create]')
        .click()

      cy.get('input[name=roomName]')
        .type("Test")

      cy.get('input[name=maxPlayers]')
        .clear()
        .type("3")

      cy.get('input[type=submit]')
        .click()
      
      cy.get('h2')
        .should('be.visible')
        .and("contain", 'Creation of room')

    })

    /* Check that you cannot create a room with a maximum number of players out of range [5-10] */
    it('Try to put max_players out of range\n [*] the room should not be created', () => {
      cy.get('button[id=create]')
        .click()

      cy.get('input[name=roomName]')
        .clear()
        .type("TestingRoom")

      cy.get('input[name=maxPlayers]')
        .clear()
        .type("0")

      cy.get('input[type=submit]')
        .click()
      
      cy.get('h2')
        .should('be.visible')
        .and("contain", 'Creation of room')

      cy.get('input[name=roomName]')
        .clear()
        .type("TestingRoom")

      cy.get('input[name=maxPlayers]')
        .clear()
        .type("-99")
      
      cy.get('input[type=submit]')
        .click()

      cy.get('h2')
        .should('be.visible')
        .and("contain", 'Creation of room')
    })

    /* Check that the user is correctly notified that not all fields are filled in. */
    it('Try to create without filling all the fields\n [*] the room should not be created', () => {
      cy.get('button[id=create]')
        .click()
      cy.on("window:alert", (alertText)  => {
        expect(alertText).eq("Please fill in all fields correctly.")
      })
      cy.get('input[type=submit]')
        .click()
    })

    /* Check that the user is correctly notified that there is a room with the chosen name. */ 
    it('Try to create with an existing room name\n [*] should not create the room and notify the user', () => {
      cy.get('button[id=create]')
        .click()
      cy.get('input[name=roomName]')
        .clear()
        .type("TestingInside")
      cy.get('input[name=maxPlayers]')
        .clear()
        .type("6")
      cy.on("window:alert", (alertText)  => {
        expect(alertText).eq("Room name already in use")
      })
      cy.get('input[type=submit]')
        .click()

    })

    /* Check that if the user clicks on "Cancel", the home page will be correctly rendered */
    it('Creation cancelled\n [*] the home page should be rendered', () => {
      cy.get('button[id=create]')
        .click()
      cy.contains('Cancel')
        .click()
      cy.get('button[id=logout]')
        .should('be.visible')
    })

    /* Check that if the fields were completed correctly and there were no problems, the room was set up correctly. */ 
    it('Everything is OK\n [*] should render the room with the user inside', () => {
      cy.get('button[id=create]')
        .click()
      cy.get('input[name=roomName]')
        .clear()
        .type("TestingInside2")
      cy.get('input[name=maxPlayers]')
        .clear()
        .type("6")
      cy.get('input[type=submit]')
        .click()
      cy.get('h1')
        .should('be.visible')
        .and("contain", 'TestingInside2')
      cy.get('h2')
        .should('be.visible')
        .and("contain", 'Lobby')
      cy.get('h3')
        .should('be.visible')
        .and("contain", 'Players in room')
      cy.wait(500)
      cy.get('ul')
        .eq('0').contains('player1')
    })

  })
})