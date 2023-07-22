package com.smallworldfs;

import com.smallworldfs.model.Transaction;
import com.smallworldfs.service.impl.TransactionService;
import lombok.RequiredArgsConstructor;

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

@RequiredArgsConstructor
public class TransactionDataFetcher {

  private final TransactionService transactionService;

  /**
   * Returns the sum of the amounts of all transactions
   */
  public double getTotalTransactionAmount() {
    try {
      return transactionService.getAllTransaction()
          .stream()
          .distinct()
          .mapToDouble(Transaction::amount)
          .sum();
    } catch (Exception e) {
      throw new UnsupportedOperationException();
    }
  }

  /**
   * Returns the sum of the amounts of all transactions sent by the specified client
   */
  public double getTotalTransactionAmountSentBy(String senderFullName) {
    try {
      return transactionService.getAllTransaction().stream().distinct()
          .filter(transaction ->
              senderFullName.equals(transaction.senderFullName()))
          .mapToDouble(Transaction::amount)
          .sum();
    } catch (Exception e) {
      throw new UnsupportedOperationException();
    }
  }

  /**
   * Returns the highest transaction amount
   */
  public double getMaxTransactionAmount() {
    try {
      return transactionService.getAllTransaction()
          .stream()
          .distinct()
          .mapToDouble(Transaction::amount)
          .max()
          .orElse(0.0);
    } catch (Exception e) {
      throw new UnsupportedOperationException();
    }
  }

  /**
   * Counts the number of unique clients that sent or received a transaction
   */
  public long countUniqueClients() {
    try {
      return transactionService.getAllTransaction()
          .stream()
          .distinct()
          .count();
    } catch (Exception e) {
      throw new UnsupportedOperationException();
    }
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
    try {
      return transactionService.getAllTransaction()
          .stream()
          .collect(Collectors.toMap(Transaction::beneficiaryFullName , Function.identity()));
    } catch (Exception e) {
      throw new UnsupportedOperationException();
    }
  }

  /**
   * Returns the identifiers of all open compliance issues
   */
  public Set<Integer> getUnsolvedIssueIds() {
    try {
      return transactionService.getAllTransaction()
          .stream()
          .filter( transaction -> Boolean.FALSE.equals(transaction.issueSolved()))
          .map(Transaction::issueId)
          .collect(Collectors.toSet());
    } catch (Exception e) {
      throw new UnsupportedOperationException();
    }
  }

  /**
   * Returns a list of all solved issue messages
   */
  public List<String> getAllSolvedIssueMessages() {
    try {
      return transactionService.getAllTransaction()
          .stream()
          .filter( transaction -> Boolean.TRUE.equals(transaction.issueSolved()))
          .map(Transaction::issueMessage)
          .collect(Collectors.toList());
    } catch (Exception e) {
      throw new UnsupportedOperationException();
    }
  }

  /**
   * Returns the 3 transactions with the highest amount sorted by amount descending
   */
  public List<Transaction> getTop3TransactionsByAmount() {
    try {
      return transactionService.getAllTransaction()
          .stream()
          .distinct()
          .sorted(Comparator.comparingDouble(Transaction::amount).reversed())
          .limit(3)
          .toList();
    } catch (Exception e) {
      throw new UnsupportedOperationException();
    }
  }

  /**
   * Returns the senderFullName of the sender with the most total sent amount
   */
  public Optional<String> getTopSender() {
    throw new UnsupportedOperationException();
  }

}
