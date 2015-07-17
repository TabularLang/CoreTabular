open System.IO


System.AppDomain.CurrentDomain.BaseDirectory


//             let dataModelConnection = model.DataModelConnection
//             let modelConnection = dataModelConnection.ModelConnection
//             let adoConnection = modelConnection.ADOConnection :?> ADODB.Connection
//
//             let query sql f =  
//               // Return a sequence of values formatted using function 'f'
//               seq { let a = adoConnection.Execute("")
//                     a.
//                     let ds = new DataSet() 
//                     let i = da.Fill(ds) 
//                     // Iterate over rows and format each row
//                     let rowCol = ds.Tables.[0].Rows 
//                     for i in 0 .. (rowCount - 1) do 
//                        yield f (rowCol.[i]) }
             