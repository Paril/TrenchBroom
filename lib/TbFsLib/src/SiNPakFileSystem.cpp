/*
 Copyright (C) 2010 Kristian Duske

 This file is part of TrenchBroom.

 TrenchBroom is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 TrenchBroom is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with TrenchBroom. If not, see <http://www.gnu.org/licenses/>.
 */

#include "fs/SiNPakFileSystem.h"

#include "fs/File.h"
#include "fs/Reader.h"
#include "fs/ReaderException.h"

#include "kd/result.h"
#include "kd/string_format.h"

namespace tb::fs
{
namespace SiNPakLayout
{
static const size_t HeaderAddress = 0x0;
static const size_t HeaderMagicLength = 0x4;
static const size_t EntryLength = 0x80;
static const size_t EntryNameLength = 0x78;
static constexpr std::string_view HeaderMagic = "SPAK";
static constexpr std::string_view HeaderMagicReloaded = "SRPK";
} // namespace SiNPakLayout

Result<void> SiNPakFileSystem::doReadDirectory()
{
  try
  {
    char magic[SiNPakLayout::HeaderMagicLength + 1] {};

    auto reader = m_file->reader();
    reader.seekForward(SiNPakLayout::HeaderAddress);
    reader.read(magic, SiNPakLayout::HeaderMagicLength);

    if (SiNPakLayout::HeaderMagic == magic) {
      const auto directoryAddress = reader.readSize<int32_t>();
      const auto directorySize = reader.readSize<int32_t>();
      const auto entryCount = directorySize / SiNPakLayout::EntryLength;

      reader.seekFromBegin(directoryAddress);

      for (size_t i = 0; i < entryCount; ++i)
      {
        const auto entryName = reader.readString(SiNPakLayout::EntryNameLength);
        const auto entryAddress = reader.readSize<int32_t>();
        const auto entrySize = reader.readSize<int32_t>();

        const auto entryPath = std::filesystem::path{kdl::str_to_lower(entryName)};
        auto entryFile_ = std::static_pointer_cast<File>(
          std::make_shared<FileView>(m_file, entryAddress, entrySize));
        addFile(
          entryPath,
          [entryFile = std::move(entryFile_)]() -> Result<std::shared_ptr<File>> {
            return entryFile;
          });
      }
    } else if (SiNPakLayout::HeaderMagicReloaded == magic) {
      reader.readInt<int32_t>(); // reserved
      // FIXME: 32-bit builds might break on this
      size_t dirofs = reader.readSize<uint64_t>();
      size_t nameofs = reader.readSize<uint64_t>();
      size_t numfiles = reader.readSize<uint32_t>();
      size_t namelen = reader.readSize<uint32_t>();

      std::string namebuffer;
      namebuffer.resize(namelen);

      reader.seekFromBegin(nameofs);
      reader.read(namebuffer.data(), namelen);

      reader.seekFromBegin(dirofs);

      for (size_t i = 0; i < numfiles; i++) {
        size_t filepos = reader.readSize<uint64_t>();
        size_t filelen = reader.readSize<uint32_t>();
        size_t nameofs = reader.readSize<uint32_t>();

        const auto entryPath = std::filesystem::path{kdl::str_to_lower(std::string_view{namebuffer.data() + nameofs, strlen(namebuffer.data() + nameofs)})};
        
        auto entryFile_ = std::static_pointer_cast<File>(
          std::make_shared<FileView>(m_file, filepos, filelen));
        addFile(
          entryPath,
          [entryFile = std::move(entryFile_)]() -> Result<std::shared_ptr<File>> {
            return entryFile;
          });
      }
    } else {
      throw Error{"bad SiN pak"};
    }

    return kdl::void_success;
  }
  catch (const ReaderException& e)
  {
    return Error{e.what()};
  }
}

} // namespace tb::fs
