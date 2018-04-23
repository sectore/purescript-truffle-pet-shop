pragma solidity ^0.4.21;

contract Adoption {

  address[16] public adopters;

  event Adopted(uint petId);

  // Adopting a pet
  function adopt(uint petId) public returns (uint) {
    require(petId >= 0 && petId <= 15);

    adopters[petId] = msg.sender;
    emit Adopted(petId);

    return petId;
  }

  // Retrieving the adopters
  function getAdopters() public view returns (address[16]) {
    return adopters;
  }

}
