context('Change username', () => {

  // https://on.cypress.io/interacting-with-elements
  describe('User is not logged in', () => {
    it('Try access to change nickname page\n [*] should render login page', () => {
      cy.visit('http://localhost:3000/change_nickname')
      cy.wait(200)
      cy.get('h2[id=subtitle]')
        .should('be.visible')
        .and('contain', "To play you need to be logged in")
    })
  })

  describe('User is logged in', () => {

    beforeEach(() => {
      // We login with a user to be able to test the component correctly
      cy.visit('/')

      cy.get('input[type=email]')
        .type('player2@example.com')
      
      cy.get('input[type=password]')
        .type('Heladera65')
  
      cy.get('input[type=submit]')
        .click()
    })

    it("Check if the home button is rendered", () => {
        cy.get('#change_nickname')
        .click()
        cy.get("#Home")
    })
    
    it('Try to change the nickname without fill the field.\n [*] should notify that situation', () => {
      cy.get('#change_nickname')
      .click()
      cy.get('#Update')
        .click()
      cy.get("#fieldValid").contains("Please enter a nickname to update it.")
    })

    it('Try to change the nickname to one that is too short.\n [*] should notify that situation', () => {
        cy.get('#change_nickname')
        .click()
        cy.get("#nameUser").type("a")
        cy.get('#Update')
        .click()
        cy.get("#nameValidation").contains("Nickname must contain between 3 and 15 characters")
    })
    
    it('Try to change the nickname too one that is too long.\n [*] should notify that situation', () => {
        cy.get('#change_nickname')
        .click()
        cy.get("#nameUser").type("abcdefghijklmnnopqrstuv")
        cy.get('#Update')
        .click()
        cy.get("#nameValidation").contains("Nickname must contain between 3 and 15 characters")
    })

    it('Try to change the nickname to one that already exists.\n [*] should notify that situation', () => {
        cy.get('#change_nickname')
        .click()
        cy.get("#nameUser").type("player0")
        cy.get('#Update')
        .click()
        cy.get("#nameUser").clear()
        cy.get("#nameUser").type("player0")
        cy.get('#Update')
        .click()
        cy.get("#badResponseDetail").contains("Nickname already registered")
    })

    it('Try to change the nickname to one that not exists.\n [*] should notify that situation and register the new nickname', () => {
        cy.get('#change_nickname')
        .click()
        cy.get("#nameUser").type("lordValdomero1")
        cy.get('#Update')
        .click()
        cy.get("#goodResponseDetail").contains("You have changed your nickname succesfully")
    })
  })

})