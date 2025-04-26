import Blob "mo:base/Blob";
import Int "mo:base/Int";
import Principal "mo:base/Principal";
module {

    public type SubAccount=Blob;
    public type Account={
        owner:Principal;
        subaccount:?SubAccount;
    };

    public type TransferArgs={
        fromSubaccount:?SubAccount;
        to:Account;
        amount:Nat;
        fee:?Nat;
        memo:?Blob;
        createdAtTime:?Int;
    };
    public type TransferError={
      #BadFee :{expectedFee:Nat};
        #BadBurn : {minBurnAmount:Nat};
        #InsufficientFunds : {balance:Nat};
        #TooOld;
        #CreatedInFuture:{ledgerTime:Int};
        #Duplicate:{duplicateOf:Nat};
        #TemporarilyUnavailable;
    };

    public type TransferKey={
        caller:Principal;
        args:TransferArgs;
    };

}