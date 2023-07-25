package com.smallworldfs.model;

import java.util.Objects;

public record Transaction(
    Integer mtn,
    double amount,
    String senderFullName,
    Integer senderAge,
    String beneficiaryFullName,
    Integer beneficiaryAge,
    Integer issueId,
    Boolean issueSolved,
    String issueMessage
) {

  /**
   * Custom implementation of equals() and hashcode() method to get distinct entries by mtn
   **/
  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    Transaction transaction = (Transaction) o;
    return Objects.equals(mtn, transaction.mtn);
  }

  @Override
  public int hashCode() {
    return Objects.hash(mtn);
  }
}
