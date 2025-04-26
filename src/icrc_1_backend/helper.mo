import Types "types";
import Blob "mo:base/Blob";
import Array "mo:base/Array";
import Principal "mo:base/Principal";
module Helper{

    func getDefaultSubAccount():Types.SubAccount{
      Blob.fromArray(Array.tabulate<Nat8>(32,func i=0))
    };

    public func getDefaultAccount(owner:Principal):Types.Account{
      {
          owner;
          subaccount=?getDefaultSubAccount();
      }
    };

    public func getAccount(account:Types.Account):Types.Account{
      switch(account.subaccount){
        case null getDefaultAccount(account.owner);
        case (?_) account
      }
    };

};