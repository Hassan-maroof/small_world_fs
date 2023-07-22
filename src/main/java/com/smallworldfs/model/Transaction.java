package com.smallworldfs.model;

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
}
