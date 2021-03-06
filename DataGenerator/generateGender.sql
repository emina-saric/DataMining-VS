USE [ohl_data]
GO

/****** Object:  StoredProcedure [dbo].[generateGender]    Script Date: 21.08.2017. 09:34:39 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

ALTER PROCEDURE [dbo].[generateGender]
AS
  BEGIN
    SET NOCOUNT ON

    DECLARE @rand_number INT, @id INT, @max_id INT

    SET @id = 1

    SELECT @max_id = COUNT(*)
    FROM OHLData

    WHILE @max_id >= @id
      BEGIN
        SET @rand_number = ABS(Checksum(NewID()) % 2)

        UPDATE OHLData
        SET Gender = (
          CASE
          WHEN @rand_number = 0
            THEN 'F'
          ELSE 'M'
          END
        )
        WHERE OHLDataID = @id

        SET @id = @id + 1
      END
  END