package com.smallworldfs.model;

public record Transaction(
    int mtn,
    double amount,
    String senderFullName,
    int senderAge,
    String beneficiaryFullName,
    int beneficiaryAge,
    int issueId,
    Boolean issueSolved,
    String issueMessage
) {
}
