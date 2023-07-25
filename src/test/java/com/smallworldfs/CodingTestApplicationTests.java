package com.smallworldfs;

import com.smallworldfs.model.Transaction;
import com.smallworldfs.service.impl.TransactionService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

@ExtendWith(MockitoExtension.class)
class TransactionDataFetcherTest {
  @Mock
  TransactionService transactionService;
  @InjectMocks
  private TransactionDataFetcher transactionDataFetcher;

  /**
   * GetTotalTransactionAmount
   **/
  @Test
  void GetTotalTransactionAmount_WhenTransactionExist() {
    Mockito.when(transactionService.getAllTransaction()).thenReturn(getAllTransactions());
    Assertions.assertEquals(1730.86, transactionDataFetcher.getTotalTransactionAmount());
  }

  @Test
  void GetTotalTransactionAmount_WhenTransactionDoNotExist() {
    Mockito.when(transactionService.getAllTransaction()).thenReturn(getEmptyTransactions());
    Assertions.assertEquals(0.0, transactionDataFetcher.getTotalTransactionAmount());
  }

  /**
   * getTotalTransactionAmountSentBy
   **/

  @Test
  void getTotalTransactionAmountSentBy_WhenTransactionExist() {
    Mockito.when(transactionService.getAllTransaction()).thenReturn(getAllTransactions());
    Assertions.assertEquals(678.06, transactionDataFetcher.getTotalTransactionAmountSentBy("Tom Shelby"));
  }

  @Test
  void getTotalTransactionAmountSentBy_WhenTransactionDoNotExist() {
    Mockito.when(transactionService.getAllTransaction()).thenReturn(getEmptyTransactions());
    double totalTransactionAmountSentBy = transactionDataFetcher.getTotalTransactionAmountSentBy("Tom Shelby");
    Assertions.assertEquals(0.0, totalTransactionAmountSentBy);
  }

  /**
   * getMaxTransactionAmount
   **/

  @Test
  void getMaxTransactionAmount_WhenTransactionExist() {
    Mockito.when(transactionService.getAllTransaction()).thenReturn(getAllTransactions());
    Assertions.assertEquals(985.0, transactionDataFetcher.getMaxTransactionAmount());
  }

  @Test
  void getMaxTransactionAmount_WhenTransactionDoNotExist() {
    Mockito.when(transactionService.getAllTransaction()).thenReturn(getEmptyTransactions());
    Assertions.assertEquals(0.0, transactionDataFetcher.getMaxTransactionAmount());
  }

  /**
   * countUniqueClients
   **/

  @Test
  void countUniqueClients_WhenTransactionExist() {
    Mockito.when(transactionService.getAllTransaction()).thenReturn(getAllTransactions());
    Assertions.assertEquals(5, transactionDataFetcher.countUniqueClients());
  }

  @Test
  void countUniqueClients_WhenTransactionDoNotExist() {
    Mockito.when(transactionService.getAllTransaction()).thenReturn(getEmptyTransactions());
    Assertions.assertEquals(0, transactionDataFetcher.countUniqueClients());
  }

  /**
   * hasOpenComplianceIssues
   **/

  @Test
  void hasOpenComplianceIssues_WhenClientHasUnresolvedIssues() {
    Mockito.when(transactionService.getAllTransaction()).thenReturn(getAllTransactions());
    Assertions.assertTrue(transactionDataFetcher.hasOpenComplianceIssues("Tom Shelby"));
  }

  @Test
  void hasOpenComplianceIssues_WhenTransactionDoNotExist() {
    Mockito.when(transactionService.getAllTransaction()).thenReturn(getEmptyTransactions());
    Assertions.assertFalse(transactionDataFetcher.hasOpenComplianceIssues("Tom Shelby"));
  }

  /**
   * getTransactionsByBeneficiaryName
   **/

  @Test
  void getTransactionsByBeneficiaryName_WhenTransactionDoNotExist() {
    Mockito.when(transactionService.getAllTransaction()).thenReturn(getEmptyTransactions());
    Assertions.assertEquals(Collections.emptyMap(), transactionDataFetcher.getTransactionsByBeneficiaryName());
  }

  /**
   * getUnsolvedIssueIds
   **/
  @Test
  void getUnsolvedIssueIds_WhenTransactionExist() {
    Mockito.when(transactionService.getAllTransaction()).thenReturn(getAllTransactions());
    Set<Integer> actualResult = transactionDataFetcher.getUnsolvedIssueIds();
    Set<Integer> expectedResult = new HashSet<>(List.of(1, 3, 15));
    Assertions.assertEquals(expectedResult, actualResult);
  }

  @Test
  void getUnsolvedIssueIds_WhenTransactionDoNotExist() {
    Mockito.when(transactionService.getAllTransaction()).thenReturn(getEmptyTransactions());
    Assertions.assertEquals(Collections.emptySet(), transactionDataFetcher.getUnsolvedIssueIds());
  }

  /**
   * getAllSolvedIssueMessages
   **/

  @Test
  void getAllSolvedIssueMessages_WhenTransactionExist() {
    Mockito.when(transactionService.getAllTransaction()).thenReturn(getAllTransactions());
    List<String> actualResult = transactionDataFetcher.getAllSolvedIssueMessages();
    List<String> expectedResult = List.of("Never gonna give you up", "Never gonna let you down");
    Assertions.assertEquals(expectedResult, actualResult);
  }

  @Test
  void getAllSolvedIssueMessages_WhenTransactionDoNotExist() {
    Mockito.when(transactionService.getAllTransaction()).thenReturn(getEmptyTransactions());
    Assertions.assertEquals(Collections.emptyList(), transactionDataFetcher.getAllSolvedIssueMessages());
  }

  /**
   * getTop3TransactionsByAmount
   **/

  @Test
  void getTop3TransactionsByAmount_WhenTransactionExist() {
    List<Transaction> transactions = getAllTransactions();
    Mockito.when(transactionService.getAllTransaction()).thenReturn(transactions);
    List<Transaction> actualResult = transactionDataFetcher.getTop3TransactionsByAmount();
    List<Transaction> expectedResult = new ArrayList<>(List.of(transactions.get(4), transactions.get(0), transactions.get(1)));
    Assertions.assertEquals(expectedResult, actualResult);
  }

  @Test
  void getTop3TransactionsByAmount_WhenTransactionDoNotExist() {
    Mockito.when(transactionService.getAllTransaction()).thenReturn(getEmptyTransactions());
    Assertions.assertEquals(Collections.emptyList(), transactionDataFetcher.getTop3TransactionsByAmount());
  }

  /**
   * getTopSender
   **/

  @Test
  void getTopSender_WhenTransactionExist() {
    Mockito.when(transactionService.getAllTransaction()).thenReturn(getAllTransactions());
    Assertions.assertEquals(Optional.of("Arthur Shelby"), transactionDataFetcher.getTopSender());
  }

  @Test
  void getTopSender_WhenTransactionDoesDoNotExist() {
    Mockito.when(transactionService.getAllTransaction()).thenReturn(getEmptyTransactions());
    Assertions.assertEquals(Optional.empty(), transactionDataFetcher.getTopSender());
  }

  /**
   * getAllTransactions() to get mock data list
   **/

  private List<Transaction> getAllTransactions() {

    Transaction transaction1 = new Transaction(
        663458,
        430.2,
        "Tom Shelby",
        22,
        "Alfie Solomons",
        33,
        1,
        false,
        "Looks like money laundering"
    );

    Transaction transaction2 = new Transaction(
        1284564,
        150.2,
        "Tom Shelby",
        22,
        "Alfie Solomons",
        60,
        2,
        true,
        "Never gonna give you up"
    );

    Transaction transaction3 = new Transaction(
        1284564,
        150.2,
        "Tom Shelby",
        22,
        "Alfie Solomons",
        60,
        3,
        false,
        "Looks like money laundering"
    );

    Transaction transaction4 = new Transaction(
        96132456,
        67.8,
        "Aunt Polly",
        34,
        "Aberama Gold",
        58,
        null,
        false,
        null
    );

    Transaction transaction5 = new Transaction(
        5465465,
        985.0,
        "Arthur Shelby",
        60,
        "Ben Younger",
        47,
        15,
        false,
        "Something's fishy"
    );

    Transaction transaction6 = new Transaction(
        1651665,
        97.66,
        "Tom Shelby",
        22,
        "Oswald Mosley",
        37,
        65,
        true,
        "Never gonna let you down"
    );

    return List.of(transaction1, transaction2, transaction3, transaction4, transaction5, transaction6);

  }

  /**
   * getEmptyTransactions() to mock Empty list
   **/

  private List<Transaction> getEmptyTransactions() {
    return Collections.emptyList();
  }

}