package com.smallworldfs.service.impl;

import com.smallworldfs.model.Transaction;

import java.io.IOException;
import java.util.List;

public interface TransactionService {
  List<Transaction> getAllTransaction() throws IOException;
}
