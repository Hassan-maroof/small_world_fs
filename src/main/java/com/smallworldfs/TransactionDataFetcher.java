package com.smallworldfs;

import com.smallworldfs.model.Transaction;
import com.smallworldfs.service.impl.TransactionService;
import lombok.RequiredArgsConstructor;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

@RequiredArgsConstructor
public class TransactionDataFetcher {

  private final TransactionService transactionService;

  /**
   * Returns the sum of the amounts of all transactions
   */
  public double getTotalTransactionAmount() {
    double totalTransactionAmount = 00.0;
    try {
      totalTransactionAmount = transactionService.getAllTransaction()
          .stream()
          .distinct()
          .mapToDouble(Transaction::amount)
          .sum();
    } catch (Exception e) {
      throw new UnsupportedOperationException();
    }
    return totalTransactionAmount;
  }

  /**
   * Returns the sum of the amounts of all transactions sent by the specified client
   */
  public double getTotalTransactionAmountSentBy(String senderFullName) {
    double totalAmountByName = 00.0;
    try {
      totalAmountByName = transactionService.getAllTransaction().stream().distinct()
          .filter(transaction ->
              senderFullName.equals(transaction.senderFullName()))
          .mapToDouble(Transaction::amount)
          .sum();
    } catch (Exception e) {
      throw new UnsupportedOperationException();
    }
    return totalAmountByName;
  }

  /**
   * Returns the highest transaction amount
   */
  public double getMaxTransactionAmount() {
    double maxTransactionAmount = 00.0;
    try {
      maxTransactionAmount = transactionService.getAllTransaction()
          .stream()
          .mapToDouble(Transaction::amount)
          .max()
          .orElse(0.0);
    } catch (Exception e) {
      throw new UnsupportedOperationException();
    }
    return maxTransactionAmount;
  }

  /**
   * Counts the number of unique clients that sent or received a transaction
   */
  public long countUniqueClients() {

    throw new UnsupportedOperationException();
  }

  /**
   * Returns whether a client (sender or beneficiary) has at least one transaction with a compliance
   * issue that has not been solved
   */
  public boolean hasOpenComplianceIssues(String clientFullName) {
    throw new UnsupportedOperationException();
  }

  /**
   * Returns all transactions indexed by beneficiary name
   */
  public Map<String, Transaction> getTransactionsByBeneficiaryName() {
    throw new UnsupportedOperationException();
  }

  /**
   * Returns the identifiers of all open compliance issues
   */
  public Set<Integer> getUnsolvedIssueIds() {
    throw new UnsupportedOperationException();
  }

  /**
   * Returns a list of all solved issue messages
   */
  public List<String> getAllSolvedIssueMessages() {
    throw new UnsupportedOperationException();
  }

  /**
   * Returns the 3 transactions with the highest amount sorted by amount descending
   */
  public List<Transaction> getTop3TransactionsByAmount() {
    throw new UnsupportedOperationException();
  }

  /**
   * Returns the senderFullName of the sender with the most total sent amount
   */
  public Optional<String> getTopSender() {
    throw new UnsupportedOperationException();
  }

}
