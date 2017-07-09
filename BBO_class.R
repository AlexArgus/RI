
######################################################################
# Create the BBO(Best Bid-Offer) class
#

setOldClass("xts")

BBO <- setClass( 
        "BBO",
        # Define the slots
        slots = c( 
                Bid   = 'xts',
                Offer = 'xts'
        )
)


# create a method to assign the Bid value 

setGeneric(name="SetBid",
           
           def=function(theObject, BidValue)
           {
                   standardGeneric("SetBid")
           }
)

setMethod(f="SetBid",
          signature="BBO",
          definition=function(theObject, BidValue)
                  
          {
                  theObject@Bid <- BidValue
                  
                  return(theObject);
          }
)

#############################

# create a method to assign the Offer value 

setGeneric(name="SetOffer",
           
           def=function(theObject, OfferValue)
           {
                   standardGeneric("SetOffer")
           }
)

setMethod(f="SetOffer",
          signature="BBO",
          definition=function(theObject, OfferValue)
                  
          {
                  theObject@Offer <- OfferValue
                  
                  return(theObject);
          }
)

#############################



Bid<- BBO();

SetBid( Bid, zB)
SetOffer( Bid, zS)


head(Bid@Bid, 40)






