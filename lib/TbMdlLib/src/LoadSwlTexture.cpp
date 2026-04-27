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

#include "mdl/LoadSwlTexture.h"

#include "fs/Reader.h"
#include "fs/ReaderException.h"
#include "gl/Texture.h"
#include "mdl/MaterialUtils.h"
#include "mdl/Palette.h"

#include "kd/result.h"

#include <string>

namespace tb::mdl
{
namespace SwlLayout
{
constexpr size_t TextureNameLength = 64;
constexpr size_t AnimNameLength = 64;
constexpr size_t MipLevels = 4;
constexpr size_t PaletteSize = 1024;
} // namespace SwlLayout


Result<gl::Texture> loadSwlTexture(fs::Reader& reader)
{
  try
  {
    reader.seekForward(SwlLayout::TextureNameLength);
    const auto width = reader.readSize<uint32_t>();
    const auto height = reader.readSize<uint32_t>();

    auto paletteReader = reader.subReaderFromCurrent(SwlLayout::PaletteSize);
    reader.seekForward(SwlLayout::PaletteSize);

    reader.seekForward(4); // palcrc

    auto offsets = std::vector<size_t>{}; // offsets from the beginning of the file

    offsets.reserve(SwlLayout::MipLevels);

    for (size_t i = 0; i < SwlLayout::MipLevels; ++i)
    {
      offsets.push_back(reader.readSize<uint32_t>());
    }

    reader.seekForward(SwlLayout::AnimNameLength);

    // Read SiN surface defaults embedded in the .swl header (sinmiptex_t layout).
    gl::SinEmbeddedDefaults sinDefaults;
    sinDefaults.flags        = reader.readInt<int32_t>();
    sinDefaults.contents     = reader.readInt<int32_t>();
    sinDefaults.value        = int(reader.readInt<uint16_t>());
    sinDefaults.direct       = int(reader.readInt<uint16_t>());
    sinDefaults.animtime     = reader.readFloat<float>();
    sinDefaults.nonlit       = reader.readFloat<float>();
    sinDefaults.directangle  = int(reader.readInt<uint16_t>());
    sinDefaults.trans_angle  = int(reader.readInt<uint16_t>());
    sinDefaults.directstyle  = reader.readFloat<float>();
    sinDefaults.translucence = reader.readFloat<float>();
    sinDefaults.friction     = reader.readFloat<float>();
    sinDefaults.restitution  = reader.readFloat<float>();
    sinDefaults.trans_mag    = reader.readFloat<float>();
    sinDefaults.color[0]     = reader.readFloat<float>();
    sinDefaults.color[1]     = reader.readFloat<float>();
    sinDefaults.color[2]     = reader.readFloat<float>();

    return mdl::loadPalette(paletteReader, mdl::PaletteColorFormat::Rgbx)
           | kdl::transform([&](const auto& palette) {
               auto mip0AverageColor = Color{};
               auto buffers = gl::TextureBufferList{};

               bool has_transparency = false;

               for (size_t mipLevel = 0; mipLevel < SwlLayout::MipLevels; ++mipLevel)
               {
                 const auto w = width / (size_t(1) << mipLevel);
                 const auto h = height / (size_t(1) << mipLevel);

                 reader.seekFromBegin(offsets[mipLevel]);

                 auto rgbaImage = gl::TextureBuffer{4 * w * h};

                 auto averageColor = Color{};
                 palette.indexedToRgba(
                   reader,
                   w * h,
                   rgbaImage,
                   mdl::PaletteTransparency::Opaque,
                   averageColor);

                 for (uint32_t i = 0; i < rgbaImage.size() / 4; i++)
                 {
                   uint32_t& c = ((uint32_t*)(rgbaImage.data()))[i];

                   // transparency
                   if (c == 0xFFFF00FF)
                   {
                     has_transparency = true;
                     c = 0;
                   }
                 }
                 buffers.emplace_back(std::move(rgbaImage));

                 if (mipLevel == 0)
                 {
                   mip0AverageColor = averageColor;
                 }
               }

               return gl::Texture{
                 width,
                 height,
                 mip0AverageColor,
                 GL_RGBA,
                 has_transparency ? gl::TextureMask::On : gl::TextureMask::Off,
                 sinDefaults,
                 std::move(buffers)};
             });
  }
  catch (const fs::ReaderException& e)
  {
    return Error{e.what()};
  }
}

} // namespace tb::mdl
