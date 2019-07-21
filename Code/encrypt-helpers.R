# Examples of sodium use via : https://cran.r-project.org/web/packages/sodium/vignettes/intro.html

##############################################################
# Saving

SaveEncrypted <- function( X, file.loc, encrypt.passphrase ) {
  # Wrapper to save objects to storage with symmetric encryption via sodium
  # -- X : an R object
  # -- file.loc : the complete filename (with full, valid path) to store (i.e. not composition of path / filename is done here)
  # -- encrypt.passphrase : character string 
  # Note each stage of the process is wrapped in silently in try() to prevent leaking data
  
  # 1 : generate encryption key : note we use key stretching via hash() to increase the derived key security
  encrypt.key <- try( sodium::hash(charToRaw(encrypt.passphrase)), silent = TRUE )
    if( class( encrypt.key ) == "try-error" ) {
      # failed to generate encryption key
      rm( list = c("encrypt.key") )
      return( "Failed generation of key while saving encrypted data" )
    }

  # 2 : serialise object to encrypt  
  d <- try( serialize(X, NULL), silent = TRUE )
  if( class( d ) == "try-error" ) {
    rm( list = c("encrypt.key", "d") )
    return( "Failed serialisation while saving encrypted data" )
  }

  # 3: Encrypt with a random nonce
  cipher <- try( sodium::data_encrypt(d, encrypt.key, nonce = sodium::random(24) ), silent = TRUE )
  if( class( cipher ) == "try-error" ) {
    rm( list = c("encrypt.key", "d", "cipher") )
    return( "Failed cipher generation while saving encrypted data" )
  } else {
    rm( list = c("encrypt.key", "d") )
  }
  
  # 4: saveRDS the encrypted data
  err.msg <- try( saveRDS( cipher, file.loc), silent = TRUE )  
  if( class( err.msg ) == "try-error" ) {
    rm( list = c("cipher","err.msg") )
    return( "Failed to save at file location while saving encrypted data")
  } else {
    rm( list = c("cipher", "err.msg") )
    return( TRUE )
  }
  
}



###############################################################
# Loading
LoadEncrypted <- function( file.loc, decrypt.passphrase ) {
  # 1 : load the encrypted file
  cipher <- try( readRDS( file.loc ), silent = TRUE )
    if( class( cipher ) == "try-error" ) {
      rm( list = c("cipher") )
      return("Failed to load data while decrypting data")
    }
  
  # 2 : generate a decryption key from the passphrase
  decrypt.key <- try( sodium::hash(charToRaw(decrypt.passphrase)), silent = TRUE )
    if( class( decrypt.key ) == "try-error" ) {
      rm( list = c("cipher","decrypt.key") )
      return("Failed generation of decryption key while decrypting data")
    }
  
  # 3 : unserialise after decrypting with key and nonce
  orig.data <- try( unserialize( data_decrypt(cipher, decrypt.key, nonce = attr(cipher, "nonce")) ), silent = TRUE )
    if( class( orig.data ) == "try-error" ) {
      rm( list = c("cipher","decrypt.key","orig.data") )
      return("Failed decryption : check passphrase")
    } else {
      rm( list = c("cipher","decrypt.key") )
      return( orig.data )
    }
}


# ## test case
# # 1 : successful save and load :
# file.loc <- paste0(getwd(), "/testing-crypto-fun.rds")
# tosave <- iris
# SaveEncrypted( tosave, file.loc, "this is a test" )
# rm( list = c("tosave"))
# 
# retrieved <- LoadEncrypted( file.loc, "this is a test")


