package com.smallworldfs.exception;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
@Getter
public enum Error {

  FILE_NOT_FOUND("10000", "File Not found");

  private final String code;
  private final String message;
}
